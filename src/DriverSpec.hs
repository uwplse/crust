{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances #-}
module DriverSpec
where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics hiding (typeOf)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

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
    | DENondet Ty
    | DEDrop DriverExpr
    | DETupleIntro [DriverExpr]
    | DETupleElim DriverExpr Int
    | DERefIntro Mutbl DriverExpr
    | DERefElim DriverExpr
  deriving (Eq, Show, Data, Typeable)

genDrivers :: Index -> Int -> [FnDesc] -> [FnDesc] -> [DriverExpr]
genDrivers ix limit lib constr = do
    (name, argTys, retTy) <- lib
    let tyParams = fn_tyParams $ runCtxM ix $ getFn name
    tyArgs <- mapM (\_ -> [TUnit, TUint (BitSize 8)]) tyParams
    let argTys' = map (subst ([], tyParams) ([], tyArgs)) argTys
    argExprs <- mapM (go (limit - 1)) argTys'
    return $ DECall name tyArgs argExprs
  where
    go limit ty | isPrimitive ty = [DENondet ty]
    go limit (TRef _ mutbl ty) = DERefIntro mutbl <$> go limit ty
    go limit (TTuple tys) = DETupleIntro <$> mapM (go limit) tys
    -- Remaining cases require fuel to operate.
    go 0 ty = []
    go limit ty = do
        (name, argTys, retTy) <- constr
        (retPart, destructOp) <- destructure retTy
        let tyParams = fn_tyParams $ runCtxM ix $ getFn name
        tyArgs <- chooseTyArgs tyParams retTy ty
        let argTys' = map (subst ([], tyParams) ([], tyArgs)) argTys
        argExprs <- mapM (go (limit - 1)) argTys'
        return $ destructOp $ DECall name tyArgs argExprs

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
destructure ty = (ty, id) : case ty of
    TRef _ _ ty' -> destructure ty'
    TTuple tys' -> concatMap destructure tys'
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
    go (DENondet ty) = return $ Expr ty $ ECall "__crust$nondet" [] [ty] []
    go (DETupleIntro des) = do
        exprs <- mapM go des
        return $ Expr (TTuple $ map typeOf exprs) $ ETupleLiteral exprs
    go (DETupleElim de idx) = do
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
    driverExpr = expandDriver' ix de
    driverTy = typeOf driverExpr

    block = Expr TUnit $ EBlock
        [ SLet (Pattern driverTy $ PVar "driver_out") (Just $ driverExpr)
        , SExpr $ withCollectedRefs ix mkBody $ Expr driverTy $ EVar "driver_out"
        ]
        (Expr TUnit $ ESimpleLiteral "unit")

    mkBody es = Expr TBottom $ ETupleLiteral es



dumpDriver depth de = case de of
    DECall name tas args -> do
        putInd $ name ++ " " ++ show (map (runPp . ppTy) tas)
        mapM_ (dumpDriver (depth + 1)) args
    DENondet ty -> putInd ("nondet " ++ show ty)
    DETupleIntro args -> do
        putInd $ "mkTuple"
        mapM_ (dumpDriver (depth + 1)) args
    DETupleElim de' i -> do
        putInd $ "tupleProj " ++ show i
        dumpDriver (depth + 1) de'
    DERefIntro mutbl de' -> do
        putInd $ "addrOf " ++ show mutbl
        dumpDriver (depth + 1) de'
    DERefElim de' -> do
        putInd $ "deref"
        dumpDriver (depth + 1) de'
  where
    putInd msg = putStrLn $ replicate (4 * depth) ' ' ++ msg
