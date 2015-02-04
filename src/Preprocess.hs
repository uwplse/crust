{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad
import Data.Char (toLower)
import Data.Generics
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
            renameLocals $
            addCleanup ix $
            renameLocals $
            liftTemps ix $
            constElim $
            ifFix $
--            fixAbort $
            fixBottom $
            fixSpecialFn $
            fixAddress $
            items'
    putStrLn $ concatMap pp items''


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

constElim items = filter (not . isConst) $ everywhere (mkT fixExpr `extT` fixPat) items
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
                [MatchArm (Pattern TBool (PSimpleLiteral "true")) e1,
                 MatchArm (Pattern TBool (PSimpleLiteral "false")) e2]) =
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

    isValid = everything (&&) (True `mkQ` goTy `extQ` goExpr)

    goTy (TAdt name _ _) = name `M.member` i_types ix
    goTy _ = True

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
