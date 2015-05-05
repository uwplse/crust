{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, Rank2Types,
        ScopedTypeVariables #-}
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (toLower)
import Data.Functor
import Data.Generics hiding (typeOf)
import Data.List (intercalate, isPrefixOf, isSuffixOf, isInfixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Numeric
import System.Environment
import Text.Parsec hiding (label, State)

import Lexer
import Parser
import Index
import TempLift hiding (fresh)
import Rename
import Pprint
import DropGlue
import Builder
import Unify
import DriverSpec
import RefSeek
import TreeShake
import Monomorphize

import Debug.Trace

dumpIr msg ir = dumpIr' ppItem msg ir

dumpIr' pp msg ir = trace text ir
  where text = "\n\n --- IR Dump (" ++ msg ++ ") ---\n\n" ++ runPp (mapM_ pp ir)

data Mode =
      MDefault
    | MRunPasses
    | MPprint
    | MPprintExpr
    | MFilter
    | MCollectUnsafe
    | MCollectTypes
    | MDriverGen
  deriving (Eq, Show)

data Config = Config
    { c_scrub :: Bool
    , c_mode :: Mode
    , c_library_filter_file :: Maybe String
    , c_construction_filter_file :: Maybe String
    , c_merged_filter_file :: Maybe String
    , c_filter_file :: String
    , c_passes :: [String]
    , c_trace_passes :: Bool
    }

defaultConfig = Config
    { c_scrub = False
    , c_mode = MDefault
    , c_library_filter_file = Nothing
    , c_construction_filter_file = Nothing
    , c_merged_filter_file = Nothing
    , c_filter_file = ""
    , c_passes = []
    , c_trace_passes = False
    }

readArgs config args = go args config
  where
    go ("--scrub" : args) config = go args $ config { c_scrub = True }
    go ("--pprint" : args) config = go args $ config { c_mode = MPprint }
    go ("--pprint-expr" : args) config = go args $ config { c_mode = MPprintExpr }
    go ("--collect-types" : args) config = go args $ config { c_mode = MCollectTypes }
    go ("--collect-unsafe" : args) config = go args $ config { c_mode = MCollectUnsafe }
    go ("--driver-gen" : args) config = go args $ config { c_mode = MDriverGen }
    go ("--library-filter" : path : args) config = go args $ config { c_library_filter_file = Just path }
    go ("--construction-filter" : path : args) config = go args $ config { c_construction_filter_file = Just path }
    go ("--merged-filter" : path : args) config = go args $ config { c_merged_filter_file = Just path }
    go ("--filter" : path : args) config = go args $ config { c_filter_file = path, c_mode = MFilter }
    go ("--passes" : passes : args) config = go args $
        config { c_passes = words $ map (\c -> case c of ',' -> ' '; c -> c) passes
               , c_mode = MRunPasses }
    go ("--trace-passes" : args) config = go args $ config { c_trace_passes = True }
    go [] config = config

main = do
    args <- getArgs
    let config = readArgs defaultConfig args

    if c_mode config == MPprintExpr then do
        exprs <- parseContents expr
        evaluate (dumpIr' ppExpr "pprint-expr" $ exprs)
        return ()
    else do

    items <- parseContents item

    case c_mode config of
        MPprint -> do
            evaluate (dumpIr "pprint" $ items)
            return ()

        MCollectUnsafe -> do
            let libraryFilterFile = fromMaybe (error $ "must set --library-filter-file") $
                    c_library_filter_file config
            libraryFilter <- (S.fromList . lines) <$> readFile libraryFilterFile
            mapM_ putStrLn $ unsafeItemNames libraryFilter items

        MCollectTypes -> do
            forM_ (collectTypes items) $ \(name, tps, args, ret) -> do
                putStrLn $ intercalate " " [name, pp tps, pp args, pp ret]

        MDriverGen -> do
            let passes = ["generate-drivers", "shake-tree"]
            items' <- runPasses config passes items
            putStrLn $ concatMap pp items'

        MFilter -> do
            content <- readFile $ c_filter_file config
            let filt = S.fromList $ lines content
            putStrLn $ concatMap pp $ filterItems items filt

        MRunPasses -> do
            items' <- runPasses config (c_passes config) items
            putStrLn $ concatMap pp items'

        MDefault -> do
            let passes = [
                    "inject-intrinsics",
                    -- Move some EBreak/EContinue into statement positions
                    -- first, since `scrub` will kill functions for having them
                    -- in non-Stmt positions.
                    "move-break",
                    "reindex",
                    if c_scrub config then "scrub" else "id",
                    "generate-default-methods",
                    "lift-strings",
                    "fix-block-ret",
                    "desugar-index",
                    "desugar-range",
                    "desugar-arg-patterns",
                    "desugar-pattern-lets",
                    "desugar-for",
                    "desugar-unsize",
                    "fix-clone",
                    "fix-address",
                    -- "fix-special-fn",
                    "fix-bottom",
                    "fix-bool",
                    "fix-if",
                    "const-expand",
                    "lift-temps",
                    "rename-locals",
                    "add-cleanup",
                    "rename-locals",
                    "filter-extern-fns",
                    "generate-drop-glues",
                    "cleanup-drops",
                    "cleanup-temps"
                    ]
            items' <- runPasses config passes items
            putStrLn $ concatMap pp items'


runPasses config passes items =
    fst <$> runPasses' config passes (items, mkIndex items)

runPasses' config passes (items, ix) = do
    tracePass config 0 "init" (items, ix)
    foldM (\(is,ix) (i,p) -> whnfList is `seq`
        runPass config (trace p p) (is, ix) >>= tracePass config i p)
        (items, ix) (zip [1..] passes)

tracePass (Config { c_trace_passes = shouldTrace }) idx pass (items, ix) = do
    when shouldTrace $
        writeFile ("pptrace-" ++ show idx ++ "-" ++ pass ++ ".ir") $ concatMap pp items
    return (items, ix)

runPass _ "reindex" (items, _) = return (items, mkIndex items)
runPass config "generate-drivers" (items, ix) = do
    (libLines, constrLines) <-
        if isJust $ c_merged_filter_file config then do
            let fileName = fromJust $ c_merged_filter_file config
            filterLines <- lines <$> readFile fileName
            return $ splitFilter filterLines
        else do
            let libFileName = fromMaybe (error "need --library-filter") $
                    c_library_filter_file config
            let constrFileName = fromMaybe (error "need --construction-filter") $
                    c_construction_filter_file config
            libLines <- lines <$> readFile libFileName
            constrLines <- lines <$> readFile constrFileName
            return (libLines, constrLines)

    return (addDrivers ix 4 (libLines, constrLines) items, ix)

runPass config "hl-generate-drivers" (items, ix) = runPasses' config passes (items, ix)
  where passes =
            [ "inject-intrinsics"
            , "move-break"
            , "reindex"
            , "scrub"
            , "generate-default-methods"
            , "reindex"
            , "scrub"
            , "generate-drivers"
            -- TODO: fix this: , "shake-tree"
            ]

runPass config "hl-clean-drivers" (items, ix) = runPasses' config passes (items, ix)
  where passes =
            [ "filter-extern-fns"
            , "filter-trait-impls"
            , "desugar-arg-patterns"
            , "desugar-pattern-lets"
            , "const-expand"
            , "desugar-unsize"
            , "desugar-range"
            , "desugar-index"
            , "fix-address"
            ]

runPass config "hl-compile-drivers" (items, ix) = runPasses' config passes (items, ix)
  where passes =
            [ "inject-intrinsics"
            , "add-driver-crust-init"
            -- Move some EBreak/EContinue into statement positions
            -- first, since `scrub` will kill functions for having them
            -- in non-Stmt positions.
            , "move-break"
            , "lift-strings"
            , "reindex"
            , "scrub"
            , "generate-default-methods"
            , "fix-block-ret"
            , "desugar-index"
            , "desugar-range"
            , "desugar-arg-patterns"
            , "desugar-pattern-lets"
            , "desugar-for"
            , "desugar-unsize"
            , "fix-clone"
            , "fix-address"
            , "fix-bool"
            , "fix-if"
            , "const-expand"
            , "lift-temps"
            , "reindex"
            , "rename-locals"
            , "add-cleanup"
            , "rename-locals"
            , "generate-drop-glues"
            , "cleanup-drops"
            , "cleanup-temps"
            , "strip-drop-glue-impls"
            -- fix-unreachable-lets requires that extern fns and TBottom return
            -- values are still around.
            , "fix-unreachable-lets"
            , "fix-bottom"
            , "filter-extern-fns"
            , "filter-trait-impls"
            ]

runPass _ pass (items, ix) = return (runBasicPass ix pass items, ix)


runBasicPass ix "desugar-index" = desugarIndex ix
runBasicPass _ "desugar-range" = desugarRange
runBasicPass _ "desugar-arg-patterns" = desugarArgPatterns
runBasicPass _ "desugar-pattern-lets" = desugarPatternLets
runBasicPass _ "desugar-for" = desugarFor
runBasicPass _ "desugar-unsize" = desugarUnsize
runBasicPass _ "fix-address" = fixAddress
runBasicPass _ "fix-special-fn" = fixSpecialFn
runBasicPass _ "fix-bottom" = fixBottom
runBasicPass _ "fix-bool" = fixBool
runBasicPass _ "fix-if" = ifFix
runBasicPass _ "fix-block-ret" = fixBlockReturn
runBasicPass _ "const-expand" = constExpand
runBasicPass ix "lift-temps" = liftTemps ix
runBasicPass ix "rename-locals" = renameLocals ix
runBasicPass ix "add-cleanup" = addCleanup ix
runBasicPass _ "filter-extern-fns" = filter (not . isExternFn)
runBasicPass _ "filter-trait-impls" = filter (not . isTraitImpl)
runBasicPass _ "generate-drop-glues" = generateDropGlues
runBasicPass _ "lift-strings" = liftStrings
runBasicPass ix "generate-default-methods" = generateDefaultMethods ix
runBasicPass _ "cleanup-drops" = cleanupDrops
runBasicPass _ "cleanup-temps" = cleanupTemps
runBasicPass _ "scrub" = scrub
runBasicPass _ "fix-clone" = fixClone
runBasicPass _ "move-break" = moveBreak
runBasicPass ix "shake-tree" = shakeTree ix
runBasicPass _ "inject-intrinsics" = (intrinsicFns ++)
runBasicPass ix "add-driver-crust-init" = addDriverCrustInit ix
runBasicPass _ "id" = id
runBasicPass _ "dump" = dumpIr "dump"
runBasicPass _ p | "dump-" `isPrefixOf` p = dumpIr (drop 5 p)
runBasicPass ix "monomorphize-from-driver-root" = monomorphize ix ["_$crust_init"]
runBasicPass _ "strip-drop-glue-impls" = stripDropGlueImpls
runBasicPass ix "fix-unreachable-lets" = fixUnreachableLets ix
runBasicPass ix "test" = \is ->
    let ty = TAdt "trait4$MyOption" [] [TInt $ BitSize 16]
    in traceShow ("has impl?", hasTraitImpl ix "trait4$Foo" [ty, ty]) is
runBasicPass _ p = error $ "unknown pass: " ++ show p

whnf x = seq x x
crush :: Data d => d -> d
crush x = runIdentity $ gfoldl
    (\c h -> crush h `seq` (c >>= \c' -> return (c' h)))
    (\c -> c `seq` Identity c)
    x
whnfList xs = foldl (\a b -> crush b `seq` a) xs xs


intrinsicFns = [nondet, assume, assert, unreachable, dropGlue]
  where
    externFn name lps tps argTys retTy = IExternFn $ ExternFnDef "intrinsic"
            name lps tps (map (\ty -> ArgDecl $ Pattern ty PWild) argTys) retTy

    nondet = externFn "__crust$nondet" [] ["T"] [] (TVar "T")
    assume = externFn "__crust$assume" [] [] [TBool] TUnit
    assert = externFn "__crust$assert" [] [] [TBool] TUnit
    unreachable = externFn "__crust$unreachable" [] [] [] TUnit

    dropGlue = IAbstractFn $ AbstractFnDef "drop_glue" [] ["T"]
                    [ArgDecl (Pattern (TRef "r_anon" MMut $ TVar "T") $ PVar "self")] TUnit


stripDropGlueImpls items = map go items
  where
    go (IFn (FnDef v n lps tps as ret (Just (ImplClause "drop_glue" _ _)) ps b)) =
        IFn (FnDef v n lps tps as ret Nothing ps b)
    go i = i


addDriverCrustInit ix items = fn : items
  where
    driverNames = filter ("$__crust_test_" `isInfixOf`) $ M.keys $ i_fns ix
    calls = map (\n -> Expr TUnit $ ECall n [] [] []) driverNames
    block = Expr (TTuple []) $ EBlock (map SExpr calls) (Expr (TTuple []) $ ETupleLiteral [])
    fn = IFn $ FnDef Public "_$crust_init" [] [] [] (TTuple []) Nothing [] block


fixClone = everywhere (mkT addDropCheckT)
  where
    addDropCheckT (IFn (FnDef v n l t arg ret impl@(Just (ImplClause "core$clone$Clone$clone" _ [ty])) preds e)) =
      let e' = addTypeCheck ty e in
      IFn (FnDef v n l t arg ret impl preds e')
    addDropCheckT e = e

    sizeOfCall ty = Expr (TInt PtrSize) (ECall "core$mem$size_of" [] [ty] [])
    dummyRetVar ty = Expr ty (EBlock [ SLet (Pattern ty (PVar "__dummy_return")) Nothing ] (Expr ty (EVar "__dummy_return")))
    mkMatch :: Ty -> Ty -> Expr -> Expr
    mkMatch ty e_ty e =
      let zero_pattern = MatchArm (Pattern (TInt PtrSize) (PSimpleLiteral "0")) (dummyRetVar e_ty) in
      let else_pattern = MatchArm (Pattern (TInt PtrSize) PWild) e in
      Expr e_ty (EBlock [] (Expr e_ty $ EMatch (sizeOfCall ty) [zero_pattern, else_pattern]))

    addTypeCheck :: Ty -> Expr -> Expr
    addTypeCheck ty expr@(Expr e_ty _) = mkMatch ty e_ty expr


cleanupDrops = everywhere (mkT cleanupDropT)
  where
    cleanupDropT (IFn (FnDef v n l t arg ret impl preds e)) =
      IFn (FnDef v n l t arg ret impl preds (walkDef e))
    cleanupDropT e = e

    walkDef = everywhere (mkT scrubDropStmt)
    scrubDropStmt (EBlock s e) =
      EBlock (filter (not . isUselessDrop) s) e
    scrubDropStmt e = e

    isUselessDrop (SExpr (Expr _ (ECall "drop_glue" _ [drop_ty] _))) = isDroplessType drop_ty
    isUselessDrop _ = False

    isDroplessType (TInt _) = True
    isDroplessType (TUint _) = True
    isDroplessType TUnit = True
    isDroplessType TBool = True
    isDroplessType (TPtr _ _) = True
    isDroplessType (TRef _ _ _) = True
    isDroplessType (TFloat _) = True
    isDroplessType TChar = True
    isDroplessType _ = False

cleanupTemps = everywhere (mkT cleanupTempT)
  where
    getUsedVars = everything S.union (S.empty `mkQ` collectUsedVars)
    collectUsedVars (EVar s) = S.singleton s
    collectUsedVars _ = S.empty
    
    cleanupTempT (IFn (FnDef v n l t arg ret impl preds e)) =
      let usedVars = getUsedVars e in
      let new_e = everywhere (mkT (scrubUselessAssign usedVars)) e in
      (IFn (FnDef v n l t arg ret impl preds new_e))
    cleanupTempT i = i

    scrubUselessAssign usedVar (EBlock s e) =
      EBlock (map (transAssign usedVar) s) e
    scrubUselessAssign usedVar e = e
    transAssign usedVars (SLet (Pattern _ (PVar v)) (Just e))
      | "lifttemp" `isPrefixOf` v && not (v `S.member` usedVars) =
          SExpr e
    transAssign _ s = s

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


fixSpecialFn items = filter (not . isAbort) $ everywhere (mkT renameAbortDef) $ everywhere (mkT renameAbortCall) items
  where
    aborts :: S.Set String
    aborts = everything S.union (S.empty `mkQ` collectAborts) items
    collectAborts (FnDef _ f_name _ _ _ _ _ _ _)
      | isSuffixOf "$crust_abort" f_name = S.singleton f_name
    collectAborts _ = S.empty
   
    canonAbort = S.findMin aborts
    
    renameAbortCall (ECall r l t e)
      | isSuffixOf "$crust_abort" r =
          ECall "crust_abort" l t e
    renameAbortCall e = e
    
    renameAbort (FnDef v f_name lp tp arg rt impl preds e)
      | f_name == canonAbort =
          (FnDef v "crust_abort" lp tp arg rt impl preds e)
    renameAbort e = e

    renameAbortDef = if S.null aborts then (\x -> x) else renameAbort

    isAbort (IFn (FnDef _ f_name _ _ _ _ _ _ _)) = isSuffixOf "$crust_abort" f_name
    isAbort _ = False

isExternFn (IExternFn _) = True
isExternFn _ = False

isTraitImpl (ITraitImpl _) = True
isTraitImpl _ = False

fixAddress = everywhere (mkT stripAddr)
  where
    stripAddr (Expr _ (EAddrOf (Expr _ (EDeref e)))) = e
    stripAddr (Expr _ (EAddrOf (Expr _ (EUnOp "UnDeref" e)))) = e
    stripAddr (Expr _ (EUnOp "UnDeref" (Expr _ (EAddrOf e)))) = e
    stripAddr (Expr _ (EDeref (Expr _ (EAddrOf e)))) = e
    stripAddr e = e

fixBlockReturn = everywhere (mkT fixupBlockRet)
  where
    fixupBlockRet (Expr ty (EBlock s ret_e@(Expr ret_ty _)))
      | ty /= ret_ty = 
         let dummy_let = SLet (Pattern ty $ PVar "_dummy_ret") Nothing in
         let dummy_ret = Expr ty (EVar "_dummy_ret") in
         let new_s = s ++ [ (SExpr ret_e), dummy_let ] in
         (Expr ty (EBlock new_s dummy_ret))
    fixupBlockRet e = e


constExpand items = go items
  where
    go :: Data a => a -> a
    go = everywhere (mkT fixExpr `extT` fixPat)

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

    fixExpr (EConst n) = case M.lookup n consts of
        Just c -> go c
        Nothing -> EVar n --error $ "no constant " ++ n ++ " for expr"
    fixExpr e = e

    fixPat (PConst n) = case M.lookup n consts of
        Just c -> exprToPat $ go c
        Nothing -> error $ "no constant " ++ n ++ " for pattern"
    fixPat e = e


ifFix = everywhere (mkT fixIf)
  where
    fixIf
        (EMatch e
                [MatchArm (Pattern TBool (PSimpleLiteral "1")) e1,
                 MatchArm (Pattern TBool (PSimpleLiteral "0")) e2]) =
         EMatch (mkCast e (TInt $ BitSize 32))
                [MatchArm (Pattern (TInt $ BitSize 32) (PSimpleLiteral "0")) e2,
                 MatchArm (Pattern (TInt $ BitSize 32) (PWild)) e1]
    fixIf x = x

-- fixAbort = everywhere (mkT go)
--   where
--     go (Expr _ (ECall "core_intrinsics_abort" _ _ _)) = Expr TUnit (ESimpleLiteral "_")
--     go x = x

fixBottom = everywhere (mkT go)
  where
    go TBottom = TUnit
    go t = t

fixUnreachableLets ix = everywhere (mkT go)
  where
    go (s@(SLet p@(Pattern pTy _) (Just e)) : ss)
      | check p e =
        traceShow ("unreachable let:", runPp . ppTy $ pTy, runPp . ppExpr $ e) $
        (SExpr e : SLet p Nothing : ss)
    go ss = ss

    check (Pattern pTy _) e =
        (computedType ix e == TUnit && pTy /= TUnit) ||
        (computedType ix e == TBottom)


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
        deref (call methodName [] [typeOf idx, typeOf arr] [arrRef, return idx])
      where
        (mutbl, methodName) = case loc of
            LvalueMut -> (MMut, "core$ops$IndexMut$index_mut")
            _ -> (MImm, "core$ops$Index$index")
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
    go (IFn (FnDef vis name lps tps args retTy impl preds body)) =
        let (args', body') = fixArgs args body
        in IFn (FnDef vis name lps tps args' retTy impl preds body')
    go (IAbstractFn (AbstractFnDef name lps tps args retTy)) =
        let args' = numberArgs args
        in IAbstractFn (AbstractFnDef name lps tps args' retTy)
    go (IExternFn (ExternFnDef abi name lps tps args retTy)) =
        let args' = numberArgs args
        in IExternFn (ExternFnDef abi name lps tps args' retTy)
    go i = i

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

desugarFor = everywhere (mkT fixFor)
    where
      fixFor (Expr ty (EFor patt@(Pattern pty _) expr@(Expr ety _) body)) = 
          let iType = iterType ety in
          let continueVar = Expr TBool$ EVar continueFlag in
          let iterVar = Expr ety $ EAddrOf (Expr iType (EVar iterTemp)) in
          let failCond = Pattern ety $ PEnum "core$option$Option" 0 [] in
          let successCond = Pattern ety (PEnum "core$option$Option" 1 [patt]) in
          let breakAssign = Expr TUnit $ EAssign continueVar $ Expr TBool $ ESimpleLiteral "0" in
          let body_e = Expr TUnit body in
          let retType = TAdt "core$option$Option" [] [pty] in
          let iterCall = Expr retType $ ECall "core$iter$Iterator$next" [] [iType] [iterVar] in
          let match = Expr TUnit $ EMatch iterCall [(MatchArm successCond body_e), (MatchArm failCond breakAssign)] in
          Expr ty (EBlock [
                    SLet (Pattern TBool (PVar continueFlag)) (Just (Expr TBool (ESimpleLiteral "1"))),
                    SLet (Pattern iType (PVar iterTemp)) (Just (Expr iType (EDeref expr)))
                   ] (Expr TUnit (EWhile continueVar match))
                  )
      fixFor e = e

      continueFlag = "__fkeepgoing"
      iterTemp = "__fitertemp"
      iterType (TRef _ _ ty) = ty
      iterType _ = error "iterator expression wasn't a reference!"


desugarPatternLets = flip evalState 0 . everywhereM (mkM goExpr)
  where
    goExpr (EBlock ss e) = do
        (ss', e') <- go ss e
        return $ EBlock ss' e'
    goExpr (EUnsafe ss e) = do
        (ss', e') <- go ss e
        return $ EUnsafe ss' e'
    goExpr e = return e

    skip s ss e = do
        (ss', e') <- go ss e
        return (s : ss', e')

    go (s@(SExpr _) : ss) e = skip s ss e
    go (s@(SLet (Pattern ty p) Nothing) : ss) e = case p of
        PVar _ -> skip s ss e
        _ -> error $ "can't handle non-PVar patterns in SLet with no Expr"
    go (s@(SLet (Pattern ty p) (Just rhs)) : ss) e = case p of
        PVar _ -> skip s ss e
        PWild -> do
            n <- fresh "__wild"
            skip (SLet (Pattern ty $ PVar n) (Just rhs)) ss e
        PTuple pats -> do
            n <- fresh "__tuple"
            let needLift = not $ hasStableLocation rhs
                topLet = SLet (Pattern ty $ PVar n) (Just rhs)
                topExpr = if needLift then Expr ty $ EVar n else rhs
                fieldLets = zipWith (\i (Pattern ty' p') -> SLet (Pattern ty' p')
                    (Just $ Expr ty' $ EField topExpr $ "field" ++ show i)) [0..] pats
            go ((if needLift then [topLet] else []) ++ fieldLets ++ ss) e
        PRefVar name -> do
            let TRef life mutbl ty' = ty
            skip (SLet (Pattern ty $ PVar name) (Just $ Expr ty $ EAddrOf rhs)) ss e
        _ -> do
            let e' = Expr (typeOf e) $ EMatch rhs
                    [MatchArm (Pattern ty p) (Expr (typeOf e) $ EBlock ss e)]
            return ([], e')

    go [] e = return ([], e)

fresh base = do
    cur <- get
    modify (+1)
    return $ base ++ show cur

liftStrings x = x'''
  where
    x' = everywhereM (mkM goExpr) x
    x'' = evalStateT x' 0
    x''' = let (a, b) = runWriter x'' in a ++ b

    goExpr orig@(Expr (TRef life mutbl TStr) (ESimpleLiteral lit)) = do
        let bytes = unhex $ drop 4 lit   -- drop "str_" prefix
        traceShow bytes $ return ()

        n <- fresh "__static_str"

        let count = length bytes
            u8 = TUint $ BitSize 8
            vecTy = TFixedVec count u8
            vecExpr = Expr vecTy $ EVec $
                map (\b -> Expr u8 $ ESimpleLiteral $ show b) bytes
        tell . (:[]) $ IStatic $ StaticDef n vecTy vecExpr

        let dataExpr = Expr (TPtr MImm u8) $ ECast $
                Expr (TRef "r_static" MImm vecTy) $ EAddrOf $
                    Expr vecTy $ EVar n
            lenExpr = Expr (TUint PtrSize) $ ESimpleLiteral $ show count
            tupleLit = ETupleLiteral [dataExpr, lenExpr]

        return $ Expr (TRef life mutbl TStr) tupleLit
    goExpr e = return e

    unhex [] = []
    unhex (a:b:xs) = (fst . head . readHex $ a:b:[]) : unhex xs

desugarUnsize = everywhere (mkT goExpr)
  where
    goExpr (Expr tyRef@(TRef l m (TVec tyElem)) (EAddrOf (Expr tyVec (EUnsizeLen len e)))) =
        let tyVecRef = TRef l m tyVec
            vecRef = Expr tyVecRef $ EAddrOf e
            dataExpr = Expr (TPtr MImm tyElem) $ ECast $ vecRef
            lenExpr = Expr (TUint PtrSize) $ ESimpleLiteral $ show len
        in Expr tyRef $ ETupleLiteral [dataExpr, lenExpr]
    goExpr e = e

generateDefaultMethods ix items = zipWith go [0..] items
  where
    go idx (IUseDefault (UseDefault lps tps (ImplClause name las tas))) = IFn def
      where 
        AbstractFnDef _ absLps absTps absArgs absRetTy = case M.lookup name (i_fns ix) of
            Just (FAbstract a) -> a
            _ -> error $ "couldn't find abstract function " ++ name

        doSubst = subst (absLps, absTps) (las, tas)

        -- Find the function-space type and lifetime parameters.  These don't
        -- appear in the IR because trans doesn't actually look at the method
        -- type.
        fnLps = drop (length las) absLps
        -- +1 for s_0, which is not included in the impl generics
        fnTps = drop (length tas) absTps

        fnLas = fnLps
        fnTas = map TVar fnTps

        name' = name ++ "$$__default" ++ show idx
        args' = zipWith (\i (ArgDecl (Pattern ty p)) ->
                ArgDecl $ Pattern (doSubst ty) (PVar $ "arg" ++ show i)) [0..] absArgs
        retTy' = doSubst absRetTy
        clause = ImplClause name (las ++ fnLas) (tas ++ fnTas)
        body = Expr retTy' $ ECall (name ++ "$$__default") (las ++ fnLas) (tas ++ fnTas) $
            map (\(ArgDecl (Pattern ty (PVar name))) -> Expr ty $ EVar name) args'
        def = FnDef Public name' (lps ++ fnLps) (tps ++ fnTps) args' retTy' (Just clause) [] $
                Expr retTy' $ EBlock [] body

    go _ i = i

filterItems items itemFilter = filter isAllowed items
    where
      isAllowed (IConst _) = True
      isAllowed (IMeta _) = True
      isAllowed (IAssociatedType _) = True
      isAllowed (IExternFn _) = True
      isAllowed (IAbstractType _) = True

      isAllowed (IStatic (StaticDef n _ _)) = S.member n itemFilter
      isAllowed (IFn (FnDef _ n _ _ _ _ _ _ _)) = S.member n itemFilter
      isAllowed (IAbstractFn (AbstractFnDef n _ _ _ _)) = S.member n itemFilter
      isAllowed (IStruct (StructDef n _ _ _ _)) = S.member n itemFilter
      isAllowed (IEnum (EnumDef n _ _ _ _)) = S.member n itemFilter

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
      (everything (&&) (True `mkQ` goTy (itemName item)) item)
      && (everything (&&) (True `mkQ` goExpr (itemName item)) item)
      && adtHasValidDrop item
      && hasAbstractFn item
      && stmtOnly (itemName item) item

    discardFor loc reason = traceShow ("discard", loc, "for", reason) False

    goTy loc (TAdt name _ _) = name `M.member` i_types ix || discardFor loc ("missing type", name)
    --goTy loc (TFixedVec _ _) = traceShow ("discard", loc, "used fixedvec") False
    goTy loc e = True

    goExpr loc (ECall name _ _ _) = name `M.member` i_fns ix || discardFor loc ("missing", name)
    goExpr loc (EConst name) = name `M.member` i_consts ix || discardFor loc ("missing", name)
    goExpr _ _ = True

    adtHasValidDrop (IStruct (StructDef loc _ _ _ (Just name))) =
        name `M.member` i_fns ix || discardFor loc ("missing drop", name)
    adtHasValidDrop (IEnum (EnumDef loc _ _ _ (Just name))) =
        name `M.member` i_fns ix || discardFor loc ("missing drop", name)
    adtHasValidDrop _ = True

    hasAbstractFn d@(IUseDefault (UseDefault _ _ (ImplClause name _ _))) =
        name `M.member` i_fns ix || discardFor (itemName d) ("missing abstract fn")
    hasAbstractFn _ = True

    -- Check for EBreak/EContinue in non-statement positions.
    stmtOnly :: Data d => String -> d -> Bool
    stmtOnly loc = (and . gmapQ (stmtOnly loc)) `extQ` stmtOnly_Stmt loc `extQ` stmtOnly_Expr_ loc

    stmtOnly_Stmt :: String -> Stmt -> Bool
    stmtOnly_Stmt loc (SExpr (Expr _ EBreak)) = True
    stmtOnly_Stmt loc (SExpr (Expr _ EContinue)) = True
    stmtOnly_Stmt loc s = and $ gmapQ (stmtOnly loc) s

    stmtOnly_Expr_ :: String -> Expr_ -> Bool
    stmtOnly_Expr_ loc EBreak = discardFor loc "non-Stmt EBreak"
    stmtOnly_Expr_ loc EContinue = discardFor loc "non-Stmt EContinue"
    stmtOnly_Expr_ loc e = and $ gmapQ (stmtOnly loc) e


moveBreak items = everywhere (mkT goMatchArm) items
  where
    goMatchArm (MatchArm pat (Expr ty EBreak)) =
        let ss = [SExpr $ Expr ty EBreak] 
            e = Expr ty $ ECall "__crust$unreachable" [] [] []
        in MatchArm pat (Expr ty $ EBlock ss e)
    goMatchArm (MatchArm pat (Expr ty EContinue)) =
        let ss = [SExpr $ Expr ty EContinue] 
            e = Expr ty $ ECall "__crust$unreachable" [] [] []
        in MatchArm pat (Expr ty $ EBlock ss e)
    goMatchArm ma = ma


unsafeItemNames filter items = mapMaybe go items
  where
    -- Only consider functions where the top-level expression is EBlock, not
    -- EUnsafe.
    go (IFn (FnDef _ name _ _ _ _ _ _ (Expr _ b@(EBlock _ _))))
      | name `S.member` filter && hasUnsafe b = Just name
    go _ = Nothing

    hasUnsafe x = everything (||) (False `mkQ` isUnsafe) x

    isUnsafe (EUnsafe _ _) = True
    isUnsafe _ = False

collectTypes items = mapMaybe go items
  where go (IFn (FnDef _ name _lps tps args retTy _ preds _)) =
            Just (name, tps, map (\(ArgDecl (Pattern ty _)) -> ty) args, retTy)
        go _ = Nothing


mkCast e t = Expr t (ECast e)

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
