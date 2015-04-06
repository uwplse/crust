{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances #-}
module RefSeek
where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Index
import Parser
import Pprint (ppTy, runPp)
import Unify


withCollectedRefs :: Index -> ([Expr] -> Expr) -> Expr -> Expr
withCollectedRefs ix mkBody input = evalState (go [] [input]) 0
  where
    go acc (e@(Expr ty _) : es) = case ty of
        TAdt name _ tas -> case fromMaybe (error $ "no type " ++ show name) $
                                M.lookup name $ i_types ix of
            -- TODO: may not be able to do this if the adt has a dtor
            TStruct (StructDef _ _ tps fields _) -> do
                let fields' = map (onFieldTy $ subst ([], tps) ([], tas)) fields
                stmts <- mapM (mkStructLet e) fields'
                expr <- go acc (map letToExpr stmts ++ es)
                return $ Expr TBottom $ EBlock stmts expr
            TEnum (EnumDef _ _ tps variants _) -> do
                let variants' = map (onVariantTy $ subst ([], tps) ([], tas)) variants
                arms <- zipWithM (mkMatchArm ty $ \es' -> go acc (es' ++ es)) [0..] variants'
                return $ Expr TBottom $ EMatch e arms
        TTuple tys -> do
            stmts <- zipWithM (mkTupleLet e) [0..] tys
            expr <- go acc (map letToExpr stmts ++ es)
            return $ Expr TBottom $ EBlock stmts expr
        TRef _ _ _ -> go (e : acc) es
        _ -> go acc es
    go acc [] = return $ mkBody acc

    onFieldTy f (FieldDef name ty) = FieldDef name (f ty)
    onVariantTy f (VariantDef name tys) = VariantDef name (map f tys)

    mkStructLet base (FieldDef name ty) = do
        varName <- fresh name
        return $ SLet (Pattern ty $ PVar varName) (Just $ Expr ty $ EField base name)

    mkTupleLet base idx ty = do
        varName <- fresh $ "tuple" ++ show idx
        return $ SLet (Pattern ty $ PVar varName) (Just $ Expr ty $ EField base $ "field" ++ show idx)

    letToExpr (SLet (Pattern ty (PVar name)) _) = Expr ty $ EVar name

    mkMatchArm ty mkBody idx (VariantDef name tys) = do
        varNames <- replicateM (length tys) (fresh name)
        let pats = zipWith (\v t -> Pattern t $ PVar v) varNames tys
        expr <- mkBody $ zipWith (\v t -> Expr t $ EVar v) varNames tys
        return $ MatchArm (Pattern ty $ PEnum name idx pats) expr

    fresh base = do
        idx <- get
        modify (+1)
        return $ "vr_" ++ base ++ "_" ++ show idx
