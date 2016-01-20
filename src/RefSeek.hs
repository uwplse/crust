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

import Builder
import Index
import Parser
import Pprint (ppTy, runPp)
import Unify


withCollectedRefs :: Index -> ([Expr] -> Expr) -> Expr -> Expr
withCollectedRefs ix mkBody input = evalState (go [] [addrOfExpr input]) 0
  where
    go acc (e@(Expr (TRef _ _ ty) _) : es) = case ty of
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
                return $ Expr TBottom $ EMatch (derefExpr e) arms
        TTuple tys -> do
            stmts <- zipWithM (mkTupleLet e) [0..] tys
            expr <- go acc (map letToExpr stmts ++ es)
            return $ Expr TBottom $ EBlock stmts expr
        TRef _ _ _ -> go (derefExpr e : acc) es
        _ -> go acc es
    go acc (e : es) = go acc es
    go acc [] = return $ mkBody acc

    onFieldTy f (FieldDef name ty) = FieldDef name (f ty)
    onVariantTy f (VariantDef name tys) = VariantDef name (map f tys)

    mkStructLet base (FieldDef name ty) = do
        varName <- fresh name
        return $ SLet (Pattern (refTy ty) $ PRefVar varName) (Just $ Expr ty $ EField base name)

    mkTupleLet base idx ty = do
        varName <- fresh $ "tuple" ++ show idx
        return $ SLet (Pattern (refTy ty) $ PRefVar varName) (Just $ Expr ty $ EField base $ "field" ++ show idx)

    letToExpr (SLet (Pattern (TRef _ _ ty) (PRefVar name)) _) = Expr ty $ EVar name

    mkMatchArm ty mkBody idx (VariantDef name tys) = do
        varNames <- replicateM (length tys) (fresh name)
        let pats = zipWith (\v t -> Pattern (refTy t) $ PRefVar v) varNames tys
        expr <- mkBody $ zipWith (\v t -> Expr (refTy t) $ EVar v) varNames tys
        return $ MatchArm (Pattern ty $ PEnum name idx pats) expr

    fresh base = do
        idx <- get
        modify (+1)
        return $ "vr_" ++ clean base ++ "_" ++ show idx

    clean name = map (\c -> case c of '$' -> '_'; _ -> c) name

    refTy t = TRef "_" MImm t
    derefExpr e = mkE ix $ deref (return e)
    addrOfExpr e = mkE ix $ addrOf' MImm (return e)

hasRef :: Index -> Ty -> Bool
hasRef ix ty = case ty of
    TVar _ -> False
    TAdt name las tas -> case i_types ix M.! name of
        TStruct (StructDef _ lps tps fields _) ->
            let fieldTys = map (\(FieldDef _ ty) -> ty) fields
                fieldTys' = map (subst (lps, tps) (las, tas)) fieldTys
            in any (hasRef ix) fieldTys'
        TEnum (EnumDef _ lps tps variants _) ->
            let argTys = concatMap (\(VariantDef _ tys) -> tys) variants
                argTys' = map (subst (lps, tps) (las, tas)) argTys
            in any (hasRef ix) argTys'
    TTuple tys -> any (hasRef ix) tys
    TRef _ _ _ -> True
    _ -> False  -- There may be refs, but withCollectedRefs can't find them.
