{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances,
             DeriveGeneric #-}
module DriverSpec
where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Array as A
import Data.Generics hiding (typeOf, Generic)
import Data.Hashable
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics (Generic)
import qualified Text.Regex.TDFA as RE
import qualified Text.Regex.TDFA.String as RE

import Debug.Trace

import Builder (typeOf)
import qualified Index
import Index hiding (getFn)
import Monomorphize (checkPreds)
import Parser
import Pprint (ppTy, runPp)
import RefSeek
import Unify


type FnDesc = (Name, [TyParam], [Ty], Ty, [Predicate])

getFnDesc :: Item -> Maybe FnDesc
getFnDesc (IFn (FnDef _ name _ tps args retTy _ preds _)) =
    Just (name, tps, map (\(ArgDecl (Pattern ty _)) -> ty) args, retTy, preds)
getFnDesc _ = Nothing


data DriverTree =
      DECall Name [Ty] [DriverTree]
    | DEMutCall Int Name [Ty] [DriverTree]
    | DENondet Ty
    | DETupleIntro [DriverTree]
    | DETupleElim Int DriverTree
    | DERefIntro Mutbl DriverTree
    | DERefElim DriverTree
    | DECopy Int
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Hashable DriverTree

data DriverExpr = DE
    { d_tree :: DriverTree
    , d_copies :: A.Array Int DriverTree
    }
  deriving (Eq, Show, Data, Typeable, Generic)

genDrivers :: Index -> Int -> [FnDesc] -> [FnDesc] -> [DriverExpr]
genDrivers ix limit lib constr =
    let lib' = filter (not . isUnsafe) lib
        unsafeLib = filter hasUnsafe lib'
        unsafeRefLib = filter producesRef unsafeLib

        constr' = filter (not . isUnsafe) constr

        drivers1 = mkDriverTrees ix limit unsafeLib constr'
        drivers2 = mkDriverTreePairs ix limit unsafeRefLib constr'
    in  traceShow ("lib sizes", length lib, length lib', length unsafeLib, length unsafeRefLib) $
        traceShow ("orig", map (\(a,_,_,_,_) -> a) lib) $
        traceShow ("safe", map (\(a,_,_,_,_) -> a) lib') $
        traceShow ("has unsafe", map (\(a,_,_,_,_) -> a) unsafeLib) $
        traceShow ("produces ref", map (\(a,_,_,_,_) -> a) unsafeRefLib) $
        traceShow ("safe constr", map (\(a,_,_,_,_) -> a) constr') $
        (drivers1 ++ drivers2) >>= addMutCalls ix limit constr' >>= addCopies
  where
    isUnsafe (f, _, _, _, _) = case i_fns ix M.! f of
        FConcrete (FnDef _ _ _ _ _ _ _ _ (Expr _ (EUnsafe _ _))) ->
            traceShow ("fn is unsafe", f) True
        FExtern _ -> traceShow ("fn is unsafe", f) $True
        _ -> False

    hasUnsafe (f, _, _, _, _) = case i_fns ix M.! f of
        FConcrete (FnDef _ _ _ _ _ _ _ _ body) ->
            let check (EUnsafe _ _) = True
                check _ = False
            in everything (||) (False `mkQ` check) body || traceShow ("fn has no unsafe", f) False
        _ -> traceShow ("fn has no unsafe (not concrete)", f) False

    producesRef (f, _, _, retTy, _) = hasRef ix retTy || traceShow ("fn produces no ref", f) False

mkDriverTrees :: Index -> Int -> [FnDesc] -> [FnDesc] -> [DriverTree]
mkDriverTrees ix limit lib constr = do
    (name, tyParams, argTys, retTy, preds) <- lib
    tyArgs <- filter (checkPreds ix tyParams preds) $
        mapM (\_ -> [TUnit, TUint (BitSize 8)]) tyParams
    genCall ix (limit + 1) constr name argTys tyArgs DECall

mkDriverTreePairs :: Index -> Int -> [FnDesc] -> [FnDesc] -> [DriverTree]
mkDriverTreePairs ix limit lib constr = do
    a <- mkDriverTrees ix limit lib constr
    b <- mkDriverTrees ix limit lib constr
    return $ DETupleIntro [a, b]

genCall :: Index -> Int -> [FnDesc] -> Name -> [Ty] -> [Ty] ->
    (Name -> [Ty] -> [DriverTree] -> DriverTree) -> [DriverTree]
genCall ix limit constr name argTys tyArgs buildCall = do
    let argTys' = map (subst ([], getTyParams ix name) ([], tyArgs)) argTys
    argExprs <- mapM (genTree ix (limit - 1) constr) argTys'
    return $ buildCall name tyArgs argExprs

genTree ix limit constr ty = go limit ty
  where
    go limit ty | isPrimitive ty = [DENondet ty]
    go limit (TRef _ mutbl ty) = DERefIntro mutbl <$> go limit ty
    go limit (TTuple tys) = DETupleIntro <$> mapM (go limit) tys
    -- Remaining cases require fuel to operate.
    go 0 ty = []
    go limit ty = do
        (name, tyParams, argTys, retTy, preds) <- constr
        (retPart, destructOp) <- destructure retTy
        tyArgs <- filter (checkPreds ix tyParams preds) $
            chooseTyArgs tyParams retPart ty
        genCall ix limit constr name argTys tyArgs
            (\n ts as -> destructOp $ DECall n ts as)

getFn ix name = runCtxM ix $ Index.getFn name
getTyParams ix = fn_tyParams . getFn ix

genMutCall ix limit constr ty dt = do
    (name, tyParams, argTys, retTy, preds) <- constr
    (argIdx, chosenTy) <- zip [0..] argTys
    -- TODO: support constructing (&mut T, _), &mut &mut T, etc, instead of
    -- just &mut T
    guard $ case chosenTy of TRef _ MMut _ -> True; _ -> False
    tyArgs <- filter (checkPreds ix tyParams preds) $
        chooseTyArgs tyParams chosenTy (TRef "r_dummy" MMut ty)

    let argTys' = map (subst ([], tyParams) ([], tyArgs)) argTys
    argExprs <- forM (zip [0..] argTys') $ \(idx, argTy) ->
        if idx == argIdx then do
            return $ DERefIntro MMut dt
        else
            genTree ix (limit - 1) constr argTy

    return $ DEMutCall argIdx name tyArgs argExprs

isPrimitive ty = case ty of
    TStr -> True
    TInt _ -> True
    TUint _ -> True
    TFloat _ -> True
    TBool -> True
    TChar -> True
    TUnit -> True
    _ -> False

destructure :: Ty -> [(Ty, DriverTree -> DriverTree)]
destructure ty = go id ty
  where
    go f ty = (ty, f) : case ty of
        TRef _ _ ty' -> go (DERefElim . f) ty'
        TTuple tys' -> concat $ zipWith (\i ty' -> go (DETupleElim i . f) ty') [0..] tys'
        _ -> []

chooseTyArgs params retTy targetTy = do
    traceShow ("try unify", retTy, targetTy) $ do
    let (uty, intern) = toUTy retTy
    result <- unify uty targetTy
    forM params $ \param ->
        case M.lookup param intern of
            Just i -> return $ getUnifiedArg result i
            Nothing -> [TUnit, TInt (BitSize 8)]


addCopies :: DriverTree -> [DriverExpr]
addCopies dt = traceShow ("before copies", dt) $ results
  where
    (nodeCount, hashList) = execState (walk goHash dt) (0, [])
      where goHash dt = do
                (idx, hs) <- get
                put (idx + 1, (idx, dt, hash dt) : hs)
                return (dt :: DriverTree) 
    hashMap :: M.Map (Int, DriverTree) [(Int, DriverTree)]
    hashMap = M.fromListWith (++) $ map (\(i,d,h) -> ((h,d), [(i,d)])) hashList

    mergeChoices :: [[[(Int, DriverTree)]]]
    mergeChoices = do
        let mergeable = filter check $ map snd $ M.toList hashMap
        concat <$> mapM (\x -> fst <$> partition x) mergeable
      where
        check xs = length xs >= 2 && checkTree (snd $ head xs)
        checkTree (DENondet _) = False
        checkTree _ = True

    results = do
        merges <- mergeChoices
        let rewriteMap = M.fromList $
                concatMap (\(ids, c) -> zip (map fst ids) (repeat c)) $ zip merges [0..]
        let doRewrite (i, d) = rewrite rewriteMap i d
        let copyList = zipWith (\ids c -> (c, doRewrite $ head ids)) merges [0..]
        let dt' = doRewrite (0, dt)

        guard (allCopiesUsedTwice (length copyList) (dt' : map snd copyList))

        let copies = A.array (0, length copyList - 1) copyList
        traceShow ("old", dt) $ traceShow ("new", dt', copies) $ do
        return $ DE dt' copies

    rewrite m base dt = evalState (walk go dt) base
      where go dt = do
                idx <- get
                modify (+1)
                if idx == base then return dt else do
                case M.lookup idx m of
                    Just copyIdx -> do
                        modify (+ (treeSize dt - 1))
                        return $ DECopy copyIdx
                    Nothing -> return dt

    allCopiesUsedTwice count dts = all ((>= 2) . snd) $ M.toList copyCounts
      where copyCounts = execState (everywhereM (mkM go) dts) $ M.fromList $ zip [0 .. count - 1] (repeat 0)
            go dt@(DECopy i) = modify (M.adjust (+1) i) >> return dt
            go dt = return dt


walk a dt = do
    dt' <- a dt
    case dt' of
        DECall x y dts' -> DECall x y <$> mapM (walk a) dts'
        DEMutCall x y z dts' -> DEMutCall x y z <$> mapM (walk a) dts'
        DENondet x -> return $ DENondet x
        DETupleIntro dts' -> DETupleIntro <$> mapM (walk a) dts'
        DETupleElim x dt' -> DETupleElim x <$> walk a dt'
        DERefIntro x dt' -> DERefIntro x <$> walk a dt'
        DERefElim dt' -> DERefElim <$> walk a dt'
        DECopy x -> return $ DECopy x

treeSize dt = execState (walk  go dt) 0
  where go dt = do
            modify (+1)
            return (dt :: DriverTree)


addMutCalls :: Index -> Int -> [FnDesc] -> DriverTree -> [DriverTree]
addMutCalls ix limit constr origDt = traceShow ("before muts", origDt) $ go (-1) origDt
  where
    callsBelow dt = case dt of
        DECall _ _ dts' -> 1 + (maximum $ 0 : map callsBelow dts')
        DEMutCall _ _ _ dts' -> error $ "unexpected DEMutCall"
        DENondet _ -> 0
        DETupleIntro dts' -> maximum $ 0 : map callsBelow dts'
        DETupleElim _ dt' -> callsBelow dt'
        DERefIntro _ dt' -> callsBelow dt'
        DERefElim dt' -> callsBelow dt'
        DECopy idx -> error $ "unexpected DECopy"

    go callsAbove dt = case dt of
        DECall name tas dts' -> traceShow ("addMutCalls/go", callsAbove, name) $ do
            let ty = getFnRetTy ix name tas
            let maxInserts = limit - (callsAbove + callsBelow dt)
            insertCount <- [0 .. maxInserts]

            dts'' <- mapM (go (callsAbove + insertCount + 1)) dts'

            dt' <- foldM (\dt idx -> addMutCall (limit - (callsAbove + idx + 1)) ty dt)
                (DECall name tas dts'') [0 .. insertCount - 1]
            return dt'

        DEMutCall _ _ _ _ -> error "unexpected DEMutCall"
        DENondet ty -> return $ DENondet ty
        DETupleIntro dts' -> DETupleIntro <$> mapM (go callsAbove) dts'
        DETupleElim idx dt' -> DETupleElim idx <$> go callsAbove dt'
        DERefIntro mutbl dt' -> DERefIntro mutbl <$> go callsAbove dt'
        DERefElim dt' -> DERefElim <$> go callsAbove dt'
        DECopy _ -> error "unexpected DECopy"

    -- TODO: add mut calls to arguments of the mut call
    addMutCall i ty dt = genMutCall ix i constr ty dt


-- Partition every subsequence into groups of two or more.  Returns the list of
-- partitions and the list of elements not in the subsequence.
partition :: [a] -> [([[a]], [a])]
partition [] = [([], [])]
partition (x:xs) = mkPart x xs ++ mkLone x xs
  where
    mkPart x xs = do
        (part, xs') <- choose [] xs
        guard (length part >= 1)
        (parts, loners) <- partition xs'
        return ((x:part):parts, loners)

    mkLone x xs = do
        (parts, loners) <- partition xs
        return (parts, x:loners)

    choose rest [] = [([], rest)]
    choose rest (x:xs) = use ++ lose
      where use = map (\(p, r) -> (x:p, r)) $ choose rest xs
            lose = map (\(p, r) -> (p, x:r)) $ choose rest xs
    



expandDriver' :: Index -> DriverExpr -> Expr
expandDriver' ix de = traceShow de $ Expr (typeOf expr) $ EBlock stmts expr
  where
    dt = d_tree de
    (expr, stmts) = evalState (runWriterT (go dt)) (0, M.empty)

    go (DECall name tyArgs argDts) = do
        argExprs <- mapM go argDts
        outVar <- fresh
        let retTy = fnRetTy name tyArgs
        let e = Expr retTy $ ECall name [] tyArgs argExprs
        tell [SLet (Pattern retTy $ PVar outVar) (Just e)]
        return $ Expr retTy $ EVar outVar
    go (DEMutCall idx name tyArgs argDts) = do
        argExprs <- mapM go argDts
        let retTy = fnRetTy name tyArgs
        tell [SExpr $ Expr retTy $ ECall name [] tyArgs argExprs]

        let mutatedExpr = argExprs !! idx
        return $ Expr (derefTy $ typeOf mutatedExpr) $ EDeref mutatedExpr
    go (DENondet ty) = return $ Expr ty $ ECall "__crust$nondet" [] [ty] []
    go (DETupleIntro dts) = do
        exprs <- mapM go dts
        return $ Expr (TTuple $ map typeOf exprs) $ ETupleLiteral exprs
    go (DETupleElim idx dt) = do
        expr <- go dt
        return $ Expr (tupleFieldTy idx $ typeOf expr) $ EField expr ("field" ++ show idx)
    go (DERefIntro mutbl dt) = do
        expr <- go dt
        return $ Expr (TRef "_" mutbl $ typeOf expr) $ EAddrOf expr
    go (DERefElim dt) = do
        expr <- go dt
        return $ Expr (derefTy $ typeOf expr) $ EDeref expr
    go (DECopy i) = do
        outVar <- fresh
        e <- goCopy i
        let retTy = typeOf e
        tell [SLet (Pattern retTy $ PVar outVar) (Just e)]
        return $ Expr retTy $ EVar outVar

    fresh = do
        idx <- gets $ \(x, _) -> x
        modify (\(x, y) -> (x + 1, y))
        return $ "v" ++ show idx

    goCopy i = do
        seen <- gets $ \(_, x) -> x
        ty <- case M.lookup i seen of
            Just ty -> return ty
            _ -> do
                expr <- go $ d_copies de A.! i
                let ty = typeOf expr
                tell [SLet (Pattern ty $ PVar $ "copy" ++ show i) (Just expr)]
                modify $ \(x, y) -> (x, M.insert i ty y)
                return ty
        return $ Expr ty $ EVar $ "copy" ++ show i


    tupleFieldTy idx (TTuple tys) = tys !! idx
    derefTy (TRef _ _ ty) = ty

    fnRetTy = getFnRetTy ix

getFnRetTy ix name tas =
    let fn = fromMaybe (error $ "no fn " ++ show name) $ M.lookup name $ i_fns ix
    in subst ([], fn_tyParams fn) ([], tas) $ fn_retTy fn


expandDriver :: Index -> DriverExpr -> Expr
expandDriver ix de = trace "expandDriver'" block
  where
    Expr driverTy (EBlock driverStmts driverExpr) = expandDriver' ix de

    block = Expr TUnit $ EBlock
        (driverStmts ++
         [ SLet (Pattern driverTy $ PVar "driver_out") (Just driverExpr)
         , SExpr $ withCollectedRefs ix mkBody $ Expr driverTy $ EVar "driver_out"
         ])
        (Expr TUnit $ ESimpleLiteral "unit")

    mkBody es = Expr TUnit $ EBlock stmts (Expr TUnit $ ESimpleLiteral "unit")
      where
        stmts = oneStmts ++ twoStmts

        oneStmts = do
            e <- es
            return $ assertNotNull e

        twoStmts = do
            (e1, e2) <- pairs es
            return $ assertNotAliased e1 e2

        assertNotNull e = SExpr $ Expr TUnit $ ECall "__crust2$assert_not_null" [] [TBottom] [e]
        assertNotAliased e1 e2 =
            SExpr $ Expr TUnit $ ECall "__crust2$assert_not_aliased" [] [TBottom, TBottom] [e1, e2]

{-
        castRef e@(Expr (TRef _ mutbl ty) _) = Expr (TPtr mutbl ty) $ ECast e
        uint = TUint PtrSize
        go e = SExpr $ Expr TUnit $ ECall "__crust$assert" [] [] [Expr TBool $
            EBinOp "BiNe" (Expr uint $ ECast $ castRef e) (Expr uint $ ESimpleLiteral "0")]
-}

        pairs [] = []
        pairs (x:xs) = zip (repeat x) xs ++ pairs xs



addDrivers :: Index -> Int -> ([String], [String]) -> [Item] -> [Item]
addDrivers ix depth (libLines, constrLines) items = items ++ drivers
  where
    libFuncs = mapMaybe getFnDesc $ filterFnsByName libLines items
    constrFuncs = mapMaybe getFnDesc $ filterFnsByName constrLines items
    driverExprs = genDrivers ix depth libFuncs constrFuncs
    drivers = map (IDriver . Driver . expandDriver ix) driverExprs



filterFnsByName filterLines items = traceShow regexStr $ filter check items
  where
    globCharToRegex c = case c of
        '*' -> ".*"
        '$' -> "\\$"
        '.' -> "\\."
        _ -> [c]
    globToRegex = concatMap globCharToRegex
    regexStrs = map globToRegex filterLines
    regexStr = "^(" ++ (tail $ concatMap ('|':) regexStrs) ++ ")$"
    regex =
        case RE.compile RE.defaultCompOpt RE.defaultExecOpt regexStr of
            Left e -> error e
            Right r -> r
    check (IFn (FnDef _ name _ _ _ _ _ _ _)) =
        case RE.execute regex name of
            Left e -> error e
            Right (Just _) -> True --traceShow ("keep", name) True
            Right Nothing -> False --traceShow ("drop", name) False
    check _ = False


splitFilter filterLines =
    (mapMaybe (go "library ") filterLines,
     mapMaybe (go "construction ") filterLines)
  where
    go prefix ln
      | prefix `isPrefixOf` ln = Just $ drop (length prefix) ln
      | otherwise = Nothing
