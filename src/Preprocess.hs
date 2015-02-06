{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, Rank2Types,
        ScopedTypeVariables #-}
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Identity
import Data.Char (toLower)
import Data.Generics hiding (typeOf)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import System.Environment
import Text.Parsec hiding (label, State)

import Lexer
import Parser
import Index
import TempLift
import Rename
import Pprint
import DropGlue
import Builder

import Debug.Trace

dumpIr msg ir = trace text ir
  where text = "\n\n --- IR Dump (" ++ msg ++ ") ---\n\n" ++ runPp (mapM_ ppItem ir)

data Config = Config
    { c_scrub :: Bool
    , c_pprint :: Bool
    , c_remove_extern :: Bool
    }

main = do
    args <- getArgs
    let (shouldScrub, pprintOnly) = case args of
            ["--scrub"] -> (True, False)
            ["--pprint"] -> (False, True)
            [] -> (False, False)
            _ -> error $ "bad command line arguments: " ++ show args

    items <- parseContents item
    if pprintOnly then evaluate (dumpIr "pprint" $ items) >> return () else do

    let items' = if shouldScrub then scrub items else items
    let ix = mkIndex items'
    let items'' =
            dumpIr "final" $
            filter (not . isExternFn) $
            fixBool $
            fixDST $
            renameLocals $
            addCleanup ix $
            renameLocals $
            liftTemps ix $
            constExpand $
            ifFix $
--            fixAbort $
            fixBottom $
            fixSpecialFn $
            fixAddress $
            desugarArgPatterns $
            desugarRange $
            desugarIndex ix $
            items'
    putStrLn $ concatMap pp items''

fixBool = everywhere (mkT fixBoolLitExpr `extT` fixBoolLitPat)
  where
    fixBoolLitExpr (Expr TBool (ESimpleLiteral b))
      | b == "true" = Expr TBool (ESimpleLiteral "1")
    fixBoolLitExpr (Expr TBool (ESimpleLiteral b))
      | b == "false" = Expr TBool (ESimpleLiteral "0")
    fixBoolLitExpr e = e

    fixBoolLitPat (Pattern TBool (PSimpleLiteral "false")) =
      Pattern TBool (PSimpleLiteral "0")
    fixBoolLitPat (Pattern TBool (PSimpleLiteral "true")) =
      Pattern TBool (PSimpleLiteral "1")
    fixBoolLitPat p = p

fixDST = everywhere (mkT transmuteDST)
  where
    transmuteDST (TRef l r TStr) = TTuple [TRef l r (TUint 8), TUint 32]
    transmuteDST (TRef l r (TVec t)) = TTuple [TRef l r t, TUint 32]
--    transmuteDST (TVec _) = error "stray vec type"
--    transmuteDST TStr = error "str str type"
    transmuteDST (TFixedVec _ _) = error "stray fixed vec type"
    transmuteDST e = e





fixSpecialFn items = filter (not . isAbort) $ everywhere (mkT renameAbortDef) $ everywhere (mkT fixInit) $ everywhere (mkT renameAbortCall) items
  where
    aborts :: S.Set String
    aborts = everything S.union (S.empty `mkQ` collectAborts) items
    collectAborts (FnDef f_name _ _ _ _ _ _)
      | isSuffixOf "_crust_abort" f_name = S.singleton f_name
    collectAborts _ = S.empty
   
    canonAbort = S.findMin aborts
    
    renameAbortCall (ECall r l t e)
      | isSuffixOf "_crust_abort" r =
          ECall "crust_abort" l t e
    renameAbortCall e = e
    
    renameAbort (FnDef f_name lp tp arg rt impl e)
      | f_name == canonAbort =
          (FnDef "crust_abort" lp tp arg rt impl e)
    renameAbort e = e

    renameAbortDef = if S.null aborts then (\x -> x) else renameAbort
    
    inits :: S.Set String
    inits = everything S.union (S.empty `mkQ` collectInits) items
    collectInits (FnDef f_name _ _ _ _ _ _) | isSuffixOf "crust_init" f_name = S.singleton f_name
    collectInits _ = S.empty

    isAbort (IFn (FnDef f_name _ _ _ _ _ _)) = isSuffixOf "_crust_abort" f_name
    isAbort _ = False

    renameInit (FnDef f_name lp tp arg rt impl e)
      | f_name == S.findMin inits = FnDef "crust_init" lp tp arg rt impl e
    renameInit e = e

    fixInit = if S.null inits then \x -> x
              else if (S.size inits) == 1 then
                     renameInit
                   else
                     error "Multiple crust_init's found!"

isExternFn (IExternFn _) = True
isExternFn _ = False

fixAddress = everywhere (mkT stripAddr)
  where
    stripAddr (Expr _ (EAddrOf (Expr _ (EDeref e)))) = e
    stripAddr (Expr _ (EAddrOf (Expr _ (EUnOp "UnDeref" e)))) = e
    stripAddr (Expr _ (EUnOp "UnDeref" (Expr _ (EAddrOf e)))) = e
    stripAddr (Expr _ (EDeref (Expr _ (EAddrOf e)))) = e
    stripAddr e = e

constExpand items = everywhere (mkT fixExpr `extT` fixPat) items
  where
    consts :: M.Map Name Expr_
    consts = everything M.union (M.empty `mkQ` collectItem) items
    collectItem (IConst (ConstDef name _ (Expr _ expr))) = M.singleton name (cleanup expr)
    collectItem _ = M.empty

    cleanup (EUnOp "UnNeg" (Expr _ (ESimpleLiteral s)))
      | head s == '-' = ESimpleLiteral $ tail s
      | otherwise = ESimpleLiteral ('-' : s)
    cleanup e = e

    isConst (IConst _) = True
    isConst _ = False

    fixExpr (EConst n) = consts M.! n
    fixExpr e = e

    fixPat (PConst n) = exprToPat $ consts M.! n
    fixPat e = e


ifFix = everywhere (mkT fixIf)
  where
    fixIf
        (EMatch e
                [MatchArm (Pattern TBool (PSimpleLiteral "1")) e1,
                 MatchArm (Pattern TBool (PSimpleLiteral "0")) e2]) =
         EMatch (mkCast e (TInt 32))
                [MatchArm (Pattern (TInt 32) (PSimpleLiteral "0")) e2,
                 MatchArm (Pattern (TInt 32) (PWild)) e1]
    fixIf x = x

-- fixAbort = everywhere (mkT go)
--   where
--     go (Expr _ (ECall "core_intrinsics_abort" _ _ _)) = Expr TUnit (ESimpleLiteral "_")
--     go x = x

fixBottom = everywhere (mkT go)
  where
    go TBottom = TUnit
    go t = t

data Location = Rvalue | Lvalue | LvalueMut
  deriving (Eq, Show, Data, Typeable)

everywhereWithLocationM :: forall m a. (Monad m, Data a) => (forall d. Data d => Location -> d -> m d) -> a -> m a
everywhereWithLocationM f x = go Rvalue x
  where
    go :: forall d. Data d => Location -> d -> m d
    go loc = (f loc <=< gmapM (go loc)) `extM` goExpr loc

    -- Process children as loc', then process e as loc
    next :: Location -> Location -> Expr -> m Expr
    next loc loc' e = f loc =<< gmapM (go loc') e

    goExpr :: Location -> Expr -> m Expr
    goExpr loc e@(Expr ty (EAddrOf _)) = do
        let loc' = case ty of
                TRef _ MMut _ -> LvalueMut
                TRef _ MImm _ -> Lvalue
                TPtr MMut _ -> LvalueMut
                TPtr MImm _ -> Lvalue
                _ -> error $ "bad type for EAddrOf: " ++ show ty
        next loc loc' e
    goExpr loc (Expr ty (EAssign l r)) = do
        ty' <- go loc ty
        l' <- go LvalueMut l
        r' <- go Rvalue r
        e_' <- f loc (EAssign l' r')
        f loc $ Expr ty' e_'
    goExpr loc (Expr ty (EAssignOp op l r)) = do
        ty' <- go loc ty
        op' <- go loc op
        l' <- go LvalueMut l
        r' <- go Rvalue r
        e_' <- f loc (EAssignOp op' l' r')
        f loc $ Expr ty' e_'
    goExpr loc e@(Expr _ (EField _ _)) = next loc loc e
    goExpr loc e@(Expr ty (EIndex arr idx)) = do
        ty' <- go loc ty
        arr' <- go loc arr
        idx' <- go Rvalue idx
        e_' <- f loc (EIndex arr' idx')
        f loc $ Expr ty' e_'
    goExpr loc e = next loc Rvalue e

everywhereWithLocation :: forall a. (Data a) => (forall d. Data d => Location -> d -> d) -> a -> a
everywhereWithLocation f = runIdentity . everywhereWithLocationM (\loc -> Identity . f loc)

desugarIndex ix = everywhereWithLocation (\loc -> mkT $ go loc)
  where
    go loc (Expr _ (EIndex arr idx)) = mkE ix $ do
        let arrRef = addrOf' mutbl (return arr)
        let idxRef = addrOf' MImm (return idx)
        deref (call methodName [] [typeOf idx, typeOf arr] [arrRef, idxRef])
      where
        (mutbl, methodName) = case loc of
            LvalueMut -> (MMut, "core_ops_IndexMut_index_mut")
            _ -> (MImm, "core_ops_Index_index")
    go loc e = e

desugarRange = everywhere (mkT go)
  where
    go (Expr ty (ERange Nothing Nothing)) =
        Expr ty $ EStructLiteral []
    go (Expr ty (ERange (Just low) Nothing)) =
        Expr ty $ EStructLiteral [Field "start" low]
    go (Expr ty (ERange Nothing (Just high))) =
        Expr ty $ EStructLiteral [Field "end" high]
    go (Expr ty (ERange (Just low) (Just high))) =
        Expr ty $ EStructLiteral [Field "start" low, Field "end" high]
    go e = e

desugarArgPatterns = map go
  where
    go (IFn (FnDef name lps tps args retTy impl body)) =
        let (args', body') = fixArgs args body
        in IFn (FnDef name lps tps args' retTy impl body')
    go (IAbstractFn (AbstractFnDef name lps tps args retTy)) =
        let args' = numberArgs args
        in IAbstractFn (AbstractFnDef name lps tps args' retTy)
    go (IExternFn (ExternFnDef abi name lps tps args retTy)) =
        let args' = numberArgs args
        in IExternFn (ExternFnDef abi name lps tps args' retTy)

    numberArgs args | all isVar args = args
    numberArgs args = zipWith (\(ArgDecl (Pattern ty _)) i ->
            ArgDecl $ Pattern ty (PVar $ "arg" ++ show i)) args [0..]

    fixArgs args body | all isVar args = (args, body)
    fixArgs args body = (args', body')
      where
        args' = numberArgs args
        stmts = zipWith (\(ArgDecl pat@(Pattern ty _)) i ->
                SLet pat (Just $ Expr ty $ EVar $ "arg" ++ show i)) args [0..]
        body' = Expr (typeOf body) $ EBlock stmts body

    isVar (ArgDecl (Pattern _ (PVar _))) = True
    isVar _ = False

data RefState = InRef | TopLevel

scrub items = scrubbed'
  where
    ix = mkIndex items

    scrubbed = filter isValid items
    scrubbed' =
        if length scrubbed < length items
            then trace ("scrub removed " ++ show (length items - length scrubbed) ++
                        " / " ++ show (length items)) $
                 scrub scrubbed
            else scrubbed

    isValid item =
      (everythingWithContext TopLevel (&&) ((\s -> (True,s)) `mkQ` goTy) item)
      && (everything (&&) (True `mkQ` goExpr) item)

    goTy (TAdt name _ _) f = (name `M.member` i_types ix,f)
    goTy (TRef l r TStr) _ = (True,InRef)
    goTy (TRef l r (TVec t)) _ = (True,InRef)
    goTy (TVec _) TopLevel = (False,TopLevel)
    goTy (TVec _) InRef = (True,InRef)
    goTy TStr TopLevel = (False,TopLevel)
    goTy TStr InRef = (True,InRef)
    goTy (TFixedVec _ _) f = (False,f)
    goTy e f = (True,f)

    goExpr (ECall name _ _ _) = name `M.member` i_fns ix
    goExpr (EConst name) = name `M.member` i_consts ix
    goExpr _ = True


mkCast e t = Expr t (ECast e t)

exprToPat (ESimpleLiteral s) = PSimpleLiteral s
exprToPat e = error $ "exprToPat: can't convert: " ++ show e


parseContents p = parseInput p <$> getContents

parseInput p = parseInput' p "<input>"

parseInput' p filename text =
    let tokens = alexScanTokens text
        items = case parse (do x <- many p; eof; return x) filename tokens of
            Left err -> error $ show err
            Right x -> x
    in items

parseFile p f = do
    result <- parseInput' p f <$> readFile f
    evaluate result
