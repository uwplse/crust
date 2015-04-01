{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances #-}
module DriverSpec
where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics
import qualified Data.Map as M
import qualified Data.Set as S

import Index
import Parser
import Pprint (ppTy, runPp)
import Unify


type FnDesc = (Name, [Ty], Ty)

getFnDesc :: Item -> Maybe FnDesc
getFnDesc (IFn (FnDef _ name _ _ args retTy _ _ _)) =
    Just (name, map (\(ArgDecl (Pattern ty _)) -> ty) args, retTy)
getFnDesc _ = Nothing


data DriverExpr =
      DECall Name [Ty] [DriverExpr]
    | DENondet
    | DETupleIntro [DriverExpr]
    | DETupleElim DriverExpr Int
    | DERefIntro DriverExpr
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
    go limit ty | isPrimitive ty = [DENondet]
    go limit (TRef _ _ ty) = DERefIntro <$> go limit ty
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


expandDriver :: DriverExpr -> Expr
expandDriver de = Expr TBottom $ EBlock stmts expr
  where
    (expr, stmts) = evalState (runWriterT (go de)) 0

    go (DECall name tyArgs argDes) = do
        argExprs <- mapM go argDes
        outVar <- mkVar
        let e = Expr TBottom $ ECall name [] tyArgs argExprs
        tell [SLet (Pattern TBottom $ PVar outVar) (Just e)]
        return $ Expr TBottom $ EVar outVar
    go DENondet = return $ Expr TBottom $ ECall "__crust$nondet" [] [TBottom] []
    go (DETupleIntro des) = do
        exprs <- mapM go des
        return $ Expr TBottom $ ETupleLiteral exprs
    go (DETupleElim de idx) = do
        expr <- go de
        return $ Expr TBottom $ EField expr ("field" ++ show idx)
    go (DERefIntro de) = do
        expr <- go de
        return $ Expr TBottom $ EAddrOf expr
    go (DERefElim de) = do
        expr <- go de
        return $ Expr TBottom $ EDeref expr

    mkVar = do
        idx <- get
        modify (+1)
        return $ "v" ++ show idx
        


dumpDriver depth de = case de of
    DECall name tas args -> do
        putInd $ name ++ " " ++ show (map (runPp . ppTy) tas)
        mapM_ (dumpDriver (depth + 1)) args
    DENondet -> putInd "nondet"
    DETupleIntro args -> do
        putInd $ "mkTuple"
        mapM_ (dumpDriver (depth + 1)) args
    DETupleElim de' i -> do
        putInd $ "tupleProj " ++ show i
        dumpDriver (depth + 1) de'
    DERefIntro de' -> do
        putInd $ "addrOf"
        dumpDriver (depth + 1) de'
    DERefElim de' -> do
        putInd $ "deref"
        dumpDriver (depth + 1) de'
  where
    putInd msg = putStrLn $ replicate (4 * depth) ' ' ++ msg
