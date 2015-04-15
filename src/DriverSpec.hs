{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances #-}
module DriverSpec
where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics hiding (typeOf)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Text.Regex.TDFA as RE
import qualified Text.Regex.TDFA.String as RE

import Debug.Trace

import Builder (typeOf)
import Index
import Parser
import Pprint (ppTy, runPp)
import RefSeek
import Unify


type FnDesc = (Name, [Ty], Ty)

getFnDesc :: Item -> Maybe FnDesc
getFnDesc (IFn (FnDef _ name _ _ args retTy _ _ _)) =
    Just (name, map (\(ArgDecl (Pattern ty _)) -> ty) args, retTy)
getFnDesc _ = Nothing


data DriverExpr =
      DECall Name [Ty] [DriverExpr]
    | DEMutCall Int Name [Ty] [DriverExpr]
    | DENondet Ty
    | DEDrop DriverExpr
    | DETupleIntro [DriverExpr]
    | DETupleElim Int DriverExpr
    | DERefIntro Mutbl DriverExpr
    | DERefElim DriverExpr
  deriving (Eq, Show, Data, Typeable)

genDrivers :: Index -> Int -> [FnDesc] -> [FnDesc] -> [DriverExpr]
genDrivers ix limit lib constr = do
    (name, argTys, retTy) <- lib
    tyArgs <- mapM (\_ -> [TUnit, TUint (BitSize 8)]) (getTyParams name)
    genCall (limit + 1) name argTys tyArgs DECall
  where
    go limit ty | isPrimitive ty = [DENondet ty]
    go limit (TRef _ mutbl ty) = DERefIntro mutbl <$> go limit ty
    go limit (TTuple tys) = DETupleIntro <$> mapM (go limit) tys
    -- Remaining cases require fuel to operate.
    go 0 ty = []
    go limit ty = do
        (name, argTys, retTy) <- constr
        (optArgIdx, chosenTy) <- zip (Nothing : map Just [0..length argTys - 1]) (retTy : argTys)
        (chosenPart, destructOp) <- destructure chosenTy
        traceShow ("consider", name, optArgIdx, chosenTy, chosenPart) $ do
        when (isJust optArgIdx) $ 
            guard $ case chosenPart of TRef _ MMut _ -> True; _ -> False
        let ty' = if isJust optArgIdx then TRef "r_dummy" MMut ty else ty
        tyArgs <- chooseTyArgs (getTyParams name) chosenPart ty'
        let buildCall name tys args = case optArgIdx of
                Nothing -> destructOp $ DECall name tys args
                Just i -> destructOp $ DERefElim $ DEMutCall i name tys args
        genCall limit name argTys tyArgs buildCall

    getFn name = runCtxM ix $ Index.getFn name
    getTyParams = fn_tyParams . getFn

    genCall limit name argTys tyArgs buildCall = do
        let tyParams = fn_tyParams $ getFn name
        let argTys' = map (subst ([], tyParams) ([], tyArgs)) argTys
        argExprs <- mapM (go (limit - 1)) argTys'
        return $ buildCall name tyArgs argExprs

isPrimitive ty = case ty of
    TStr -> True
    TInt _ -> True
    TUint _ -> True
    TFloat _ -> True
    TBool -> True
    TChar -> True
    TUnit -> True
    _ -> False

destructure :: Ty -> [(Ty, DriverExpr -> DriverExpr)]
destructure ty = go id ty
  where
    go f ty = (ty, f) : case ty of
        TRef _ _ ty' -> go (DERefElim . f) ty'
        TTuple tys' -> concat $ zipWith (\i ty' -> go (DETupleElim i . f) ty') [0..] tys'
        _ -> []

chooseTyArgs params retTy targetTy = do
    let (uty, intern) = toUTy retTy
    result <- unify uty targetTy
    forM params $ \param ->
        case M.lookup param intern of
            Just i -> return $ getUnifiedArg result i
            Nothing -> [TUnit, TInt (BitSize 8)]


expandDriver' :: Index -> DriverExpr -> Expr
expandDriver' ix de = Expr (typeOf expr) $ EBlock stmts expr
  where
    (expr, stmts) = evalState (runWriterT (go de)) 0

    go (DECall name tyArgs argDes) = do
        argExprs <- mapM go argDes
        outVar <- fresh
        let retTy = fnRetTy name tyArgs
        let e = Expr retTy $ ECall name [] tyArgs argExprs
        tell [SLet (Pattern retTy $ PVar outVar) (Just e)]
        return $ Expr retTy $ EVar outVar
    go (DEMutCall idx name tyArgs argDes) = do
        argExprs <- mapM go argDes
        let retTy = fnRetTy name tyArgs
        tell [SExpr $ Expr retTy $ ECall name [] tyArgs argExprs]
        return $ argExprs !! idx
    go (DENondet ty) = return $ Expr ty $ ECall "__crust$nondet" [] [ty] []
    go (DETupleIntro des) = do
        exprs <- mapM go des
        return $ Expr (TTuple $ map typeOf exprs) $ ETupleLiteral exprs
    go (DETupleElim idx de) = do
        expr <- go de
        return $ Expr (tupleFieldTy idx $ typeOf expr) $ EField expr ("field" ++ show idx)
    go (DERefIntro mutbl de) = do
        expr <- go de
        return $ Expr (TRef "_" mutbl $ typeOf expr) $ EAddrOf expr
    go (DERefElim de) = do
        expr <- go de
        return $ Expr (derefTy $ typeOf expr) $ EDeref expr

    fresh = do
        idx <- get
        modify (+1)
        return $ "v" ++ show idx

    fnRetTy name tas =
        let fn = fromMaybe (error $ "no fn " ++ show name) $ M.lookup name $ i_fns ix
        in subst ([], fn_tyParams fn) ([], tas) $ fn_retTy fn

    tupleFieldTy idx (TTuple tys) = tys !! idx
    derefTy (TRef _ _ ty) = ty



expandDriver :: Index -> DriverExpr -> Expr
expandDriver ix de = block
  where
    Expr driverTy (EBlock driverStmts driverExpr) = expandDriver' ix de

    block = Expr TUnit $ EBlock
        (driverStmts ++
         [ SLet (Pattern driverTy $ PVar "driver_out") (Just driverExpr)
         , SExpr $ withCollectedRefs ix mkBody $ Expr driverTy $ EVar "driver_out"
         ])
        (Expr TUnit $ ESimpleLiteral "unit")

    mkBody es = Expr TUnit $ EBlock (map go es) (Expr TUnit $ ESimpleLiteral "unit")
      where
        castRef e@(Expr (TRef _ mutbl ty) _) = Expr (TPtr mutbl ty) $ ECast e
        uint = TUint PtrSize
        go e = SExpr $ Expr TUnit $ ECall "__crust$assert" [] [] [Expr TBool $
            EBinOp "BiNe" (Expr uint $ ECast $ castRef e) (Expr uint $ ESimpleLiteral "0")]



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
            Right (Just _) -> traceShow ("keep", name) True
            Right Nothing -> traceShow ("drop", name) False
    check _ = False


splitFilter filterLines =
    (mapMaybe (go "library ") filterLines,
     mapMaybe (go "construction ") filterLines)
  where
    go prefix ln
      | prefix `isPrefixOf` ln = Just $ drop (length prefix) ln
      | otherwise = Nothing
