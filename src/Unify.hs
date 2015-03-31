{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleInstances, OverlappingInstances #-}
module Unify
where

import Control.Monad
import Control.Monad.State
import Data.Generics
import qualified Data.Map as M

import Parser


data UTy =
      UVar Int Name
    | UCtor Name [UTy] ([Ty] -> Ty)
  deriving (Data, Typeable)

instance Show UTy where
    show (UVar i name) = "UVar " ++ show i ++ " " ++ show name
    show (UCtor name utys _) = "UCtor " ++ show name ++ " " ++ show utys

type VarIntern = M.Map Name Int
type UnifyResult = M.Map Int UTy

toUTy :: Ty -> (UTy, VarIntern)
toUTy ty = runState (go ty) M.empty
  where
    intern name = do
        map <- get
        case M.lookup name map of
            Just num -> return num
            Nothing -> do
                let num = M.size map
                let map' = M.insert name num map
                put map'
                return num

    go (TVar name) = intern name >>= \num -> return $ UVar num name
    go (TAdt name las tas) = do
        u_tas <- mapM go tas
        return $ UCtor ("adt_" ++ name) u_tas (TAdt name las)
    go (TTuple tys) = do
        u_tys <- mapM go tys
        return $ UCtor "tuple" u_tys TTuple
    go (TRef life mutbl ty) = do
        u_ty <- go ty
        return $ UCtor ("ref_" ++ show mutbl) [u_ty] (TRef life mutbl . head)
    go (TPtr mutbl ty) = do
        u_ty <- go ty
        return $ UCtor ("ptr_" ++ show mutbl) [u_ty] (TPtr mutbl . head)
    go TStr = return $ UCtor "str" [] (const TStr)
    go (TVec ty) = do
        u_ty <- go ty
        return $ UCtor "vec" [u_ty] (TVec . head)
    go (TFixedVec n ty) = do
        u_ty <- go ty
        return $ UCtor ("vec_" ++ show n) [u_ty] (TFixedVec n . head)
    go (TInt n) = return $ UCtor ("int_" ++ show n) [] (const $ TInt n)
    go (TUint n) = return $ UCtor ("uint_" ++ show n) [] (const $ TUint n)
    go (TFloat n) = return $ UCtor ("float_" ++ show n) [] (const $ TFloat n)
    go TBool = return $ UCtor "bool" [] (const TBool)
    go TChar = return $ UCtor "bool" [] (const TChar)
    go TFn = return $ UCtor "fn" [] (const TFn)
    go TUnit = return $ UCtor "unit" [] (const TUnit)
    go TBottom = return $ UCtor "bottom" [] (const TBottom)
    go (TAbstract name las tas) = error $ "can't handle abstract UTy"

fromUTy :: UnifyResult -> UTy -> Ty
fromUTy m (UVar i _) = fromUTy m (m M.! i)
fromUTy m (UCtor name utys mk) = mk $ map (fromUTy m) utys

unify :: UTy -> Ty -> [UnifyResult]
unify uty ty = go M.empty uty (fst $ toUTy ty)
  where
    go m _ (UVar _ _) = error $ "concrete type should not contain TVar!"
    go m (UVar i _) conc
      | Just conc' <- M.lookup i m = go m conc' conc
      | otherwise = [M.insert i conc m]
    go m (UCtor name args _) (UCtor name' args' _)
      | name /= name' = []
      | otherwise = goZip m args args'

    goZip m [] [] = [m]
    goZip m (x:xs) (y:ys) = do
        m' <- go m x y
        goZip m' xs ys
