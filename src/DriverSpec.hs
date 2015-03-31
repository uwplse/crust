{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances #-}
module DriverSpec
where

import Control.Applicative ((<$>))
import Control.Monad
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
    | DEPtrIntro DriverExpr
  deriving (Eq, Show, Data, Typeable)

genDrivers :: Int -> [FnDesc] -> [FnDesc] -> [DriverExpr]
genDrivers limit lib constr = do
    (name, argTys, retTy) <- lib
    let tyParams = collectParams (retTy : argTys)
    tyArgs <- mapM (\_ -> [TUnit, TUint (BitSize 8)]) tyParams
    let argTys' = map (subst ([], tyParams) ([], tyArgs)) argTys
    argExprs <- mapM (go (limit - 1)) argTys'
    return $ DECall name tyArgs argExprs
  where
    go limit ty | isPrimitive ty = [DENondet]
    go limit (TRef _ _ ty) = DERefIntro <$> go limit ty
    go limit (TPtr _ ty) = DEPtrIntro <$> go limit ty
    go limit (TTuple tys) = DETupleIntro <$> mapM (go limit) tys
    -- Remaining cases require fuel to operate.
    go 0 ty = []
    go limit ty = do
        (name, argTys, retTy) <- constr
        (retPart, destructOp) <- destructure retTy
        let tyParams = collectParams (retTy : argTys)
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

collectParams tys = S.toList $ everything (S.union) (S.empty `mkQ` go) tys
  where
    go (TVar n) = S.singleton n
    go _ = S.empty

chooseTyArgs params retTy targetTy = do
    let (uty, intern) = toUTy retTy
    result <- unify uty targetTy
    forM params $ \param ->
        case M.lookup param intern of
            Just i -> return $ getUnifiedArg result i
            Nothing -> [TUnit, TInt (BitSize 8)]


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
    DEPtrIntro de' -> do
        putInd $ "ptrAddrOf"
        dumpDriver (depth + 1) de'
  where
    putInd msg = putStrLn $ replicate (4 * depth) ' ' ++ msg
