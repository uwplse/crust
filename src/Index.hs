{-# LANGUAGE DeriveDataTypeable #-}
module Index
where

import Control.Monad.Reader
import Data.Generics
import qualified Data.Map as M
import Data.Maybe

import Parser


-- TODO: move this somewhere more appropriate

subst :: ([LifetimeParam], [TyParam]) -> ([Lifetime], [Ty]) -> Ty -> Ty
subst (lp, tp) (la, ta) t = goTy t
  where
    lifeMap = M.fromList $ zip lp la
    tyMap = M.fromList $ zip tp ta

    goTy (TVar n) = case M.lookup n tyMap of Just t -> t; Nothing -> TVar n
    goTy (TAdt n ls ts) = TAdt n (map goLife ls) (map goTy ts)
    goTy (TTuple ts) = TTuple (map goTy ts)
    goTy (TRef l m t) = TRef (goLife l) m (goTy t)
    goTy (TPtr m t) = TPtr m (goTy t)
    goTy t = t

    goLife n = case M.lookup n lifeMap of Just l -> l; Nothing -> n


data TypeDef =
      TStruct StructDef
    | TEnum EnumDef
  deriving (Eq, Show, Data, Typeable)

ty_name (TStruct (StructDef n _ _ _ _)) = n
ty_name (TEnum (EnumDef n _ _ _ _)) = n

ty_lifetimeParams (TStruct (StructDef _ ps _ _ _)) = ps
ty_lifetimeParams (TEnum (EnumDef _ ps _ _ _)) = ps

ty_tyParams (TStruct (StructDef _ _ ps _ _)) = ps
ty_tyParams (TEnum (EnumDef _ _ ps _ _)) = ps

ty_dtor (TStruct (StructDef _ _ _ _ d)) = d
ty_dtor (TEnum (EnumDef _ _ _ _ d)) = d

fn_lifetimeParams (FnDef _ ps _ _ _ _) = ps
fn_tyParams (FnDef _ _ ps _ _ _) = ps
fn_retTy (FnDef _ _ _ _ r _) = r

data Index = Index
    { i_fns :: M.Map Name FnDef
    , i_types :: M.Map Name TypeDef
    , i_consts :: M.Map Name ConstDef
    }

mkIndex items = Index fns types consts
  where
    fns = M.fromList $ dropGlue : mapMaybe onFn items
    types = M.fromList $ mapMaybe onType items
    consts = M.fromList $ mapMaybe onConst items

    onFn (IFn x@(FnDef name _ _ _ _ _)) = Just (name, x)
    onFn _ = Nothing

    onType (IStruct x@(StructDef name _ _ _ _)) = Just (name, TStruct x)
    onType (IEnum x@(EnumDef name _ _ _ _)) = Just (name, TEnum x)
    onType _ = Nothing

    onConst (IConst x@(ConstDef name _ _)) = Just (name, x)
    onConst _ = Nothing

dropGlue = ("drop_glue", def)
  where def = FnDef "drop_glue" [] ["T"]
                [ArgDecl "self" (TRef "r_anon" MMut $ TVar "T")]
                TUnit (Expr TUnit $ ESimpleLiteral "drop_glue")


type CtxM a = Reader Index a

runCtxM ix a = runReader a ix

getFn name = do
    mx <- asks $ M.lookup name . i_fns
    case mx of
        Just x -> return x
        Nothing -> error $ "unknown fn: " ++ name

getType name = do
    mx <- asks $ M.lookup name . i_types
    case mx of
        Just x -> return x
        Nothing -> error $ "unknown type: " ++ name

getConst name = do
    mx <- asks $ M.lookup name . i_consts
    case mx of
        Just x -> return x
        Nothing -> error $ "unknown const: " ++ name

getStruct name = do
    ty <- getType name
    case ty of
        TStruct x -> return x
        TEnum _ -> error $ "type " ++ name ++ " is an enum, not a struct"

getEnum name = do
    ty <- getType name
    case ty of
        TEnum x -> return x
        TStruct _ -> error $ "type " ++ name ++ " is a struct, not an enum"

getAdtDtor name = do
    ty <- getType name
    case ty of
        TStruct (StructDef _ _ _ _ dtor) -> return dtor
        TEnum (EnumDef _ _ _ _ dtor) -> return dtor

data Kind = Copy | Linear
  deriving (Eq, Show, Data, Typeable)

combineKinds Copy Copy = Copy
combineKinds _ _ = Linear

-- TODO: TVar case should look for Copy trait bound
getKind (TVar _) = return Linear
getKind (TAdt name las tas) = do
    ty <- getType name
    if isJust (ty_dtor ty) then return Linear else do

    let tys = collectElementTypes ty las tas
    kinds <- mapM getKind tys
    return $ foldl combineKinds Copy kinds
  where
    collectElementTypes (TStruct (StructDef _ lps tps fields _)) las tas =
        map (subst (lps, tps) (las, tas)) $
        map (\(FieldDef _ ty) -> ty) fields
    collectElementTypes (TEnum (EnumDef _ lps tps variants _)) las tas =
        map (subst (lps, tps) (las, tas)) $
        concatMap (\(VariantDef _ tys) -> tys) variants
getKind (TTuple tys) = do
    kinds <- mapM getKind tys
    return $ foldl combineKinds Copy kinds
getKind (TRef _ MMut _) = return Linear
getKind (TRef _ MImm _) = return Copy
getKind (TPtr _ _) = return Copy
getKind _ = return Copy


getFieldTy (StructDef structName _ _ fs _) n = case candidates of
    [FieldDef _ ty] -> ty
    [] -> error $ "struct " ++ structName ++ " has no field called " ++ n
    _ -> error $ "struct " ++ structName ++ " has multiple fields called " ++ n
  where
    candidates = filter (\(FieldDef n' _) -> n' == n) fs

adtName ty = case ty of
    TAdt name _ _ -> name
    t -> error $ "expected self to be TAdt, not " ++ show t
