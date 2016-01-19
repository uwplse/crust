{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Index
where

import Control.Monad.Reader
import Data.Generics
import qualified Data.Map as M
import Data.Maybe

import Parser

import Debug.Trace


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
    goTy (TVec t) = TVec (goTy t)
    goTy (TFixedVec n t) = TFixedVec n (goTy t)
    goTy (TAbstract n ls ts) = TAbstract n (map goLife ls) (map goTy ts)
    goTy t = t

    goLife n = case M.lookup n lifeMap of Just l -> l; Nothing -> n

substPred :: ([LifetimeParam], [TyParam]) -> ([Lifetime], [Ty]) -> Predicate -> Predicate
substPred (lp, tp) (la, ta) p =
    let go = subst (lp, tp) (la, ta)
    in case p of
        PImpl name tys -> PImpl name (map go tys)
        PEq ty1 ty2 -> PEq (go ty1) (go ty2)

data AnyFnDef =
      FConcrete FnDef
    | FAbstract AbstractFnDef
    | FExtern ExternFnDef
  deriving (Eq, Show, Data, Typeable)

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

fn_name (FConcrete (FnDef _ name _ _ _ _ _ _ _)) = name
fn_name (FAbstract (AbstractFnDef name _ _ _ _)) = name
fn_name (FExtern (ExternFnDef _ name _ _ _ _)) = name
fn_lifetimeParams (FConcrete (FnDef _ _ ps _ _ _ _ _ _)) = ps
fn_lifetimeParams (FAbstract (AbstractFnDef _ ps _ _ _)) = ps
fn_lifetimeParams (FExtern (ExternFnDef _ _ ps _ _ _)) = ps
fn_tyParams (FConcrete (FnDef _ _ _ ps _ _ _ _ _)) = ps
fn_tyParams (FAbstract (AbstractFnDef _ _ ps _ _)) = ps
fn_tyParams (FExtern (ExternFnDef _ _ _ ps _ _)) = ps
fn_args (FConcrete (FnDef _ _ _ _ as _ _ _ _)) = as
fn_args (FAbstract (AbstractFnDef _ _ _ as _)) = as
fn_args (FExtern (ExternFnDef _ _ _ _ as _)) = as
fn_argTys f = map (\(ArgDecl (Pattern ty _)) -> ty) $ fn_args f
fn_retTy (FConcrete (FnDef _ _ _ _ _ r _ _ _)) = r
fn_retTy (FAbstract (AbstractFnDef _ _ _ _ r)) = r
fn_retTy (FExtern (ExternFnDef _ _ _ _ _ r)) = r

data Index = Index
    { i_fns :: M.Map Name AnyFnDef
    , i_types :: M.Map Name TypeDef
    , i_consts :: M.Map Name ConstDef
    , i_statics :: M.Map Name StaticDef

    , i_assoc_tys :: M.Map Name [AssociatedTypeDef]
    , i_impls :: M.Map Name [TraitImpl]
    }

mkIndex items = Index fns types consts statics assocTys impls
  where
    fns = M.fromList $ mapMaybe onFn items
    types = M.fromList $ mapMaybe onType items
    consts = M.fromList $ mapMaybe onConst items
    statics = M.fromList $ mapMaybe onStatic items
    assocTys = M.fromListWith (++) $ mapMaybe onAssociatedType items
    impls = M.fromListWith (++) $ mapMaybe onImpl items

    onFn (IFn x@(FnDef _ name _ _ _ _ _ _ _)) = Just (name, FConcrete x)
    onFn (IAbstractFn x@(AbstractFnDef name _ _ _ _)) = Just (name, FAbstract x)
    onFn (IExternFn x@(ExternFnDef _ name _ _ _ _)) = Just (name, FExtern x)
    onFn _ = Nothing

    onType (IStruct x@(StructDef name _ _ _ _)) = Just (name, TStruct x)
    onType (IEnum x@(EnumDef name _ _ _ _)) = Just (name, TEnum x)
    onType _ = Nothing

    onConst (IConst x@(ConstDef name _ _)) = Just (name, x)
    onConst _ = Nothing

    onStatic (IStatic x@(StaticDef name _ _)) = Just (name, x)
    onStatic _ = Nothing

    onAssociatedType (IAssociatedType x@(AssociatedTypeDef _ _ (ImplClause name _ _) _)) =
        Just (name, [x])
    onAssociatedType _ = Nothing

    onImpl (ITraitImpl x@(TraitImpl _ _ (ImplClause name _ _) _)) = Just (name, [x])
    onImpl _ = Nothing

assocTys ix name = fromMaybe [] $ M.lookup name (i_assoc_tys ix)
traitImpls ix name = fromMaybe [] $ M.lookup name (i_impls ix)



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

getStatic name = do
    mx <- asks $ M.lookup name . i_statics
    case mx of
        Just x -> return x
        Nothing -> error $ "unknown statics: " ++ name

getConcreteFn name = do
    fn <- getFn name
    case fn of
        FConcrete x -> return x
        FAbstract _ -> error $ "type " ++ name ++ " is abstract, not concrete"

getAbstractFn name = do
    fn <- getFn name
    case fn of
        FAbstract x -> return x
        FConcrete _ -> error $ "type " ++ name ++ " is concrete, not abstract"

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


itemName (IStruct (StructDef name _ _ _ _)) = name
itemName (IEnum (EnumDef name _ _ _ _)) = name
itemName (IConst (ConstDef name _ _)) = name
itemName (IFn (FnDef _ name _ _ _ _ _ _ _)) = name
itemName (IAbstractFn (AbstractFnDef name _ _ _ _)) = name
itemName (IExternFn (ExternFnDef _ name _ _ _ _)) = name
itemName (IAbstractType (AbstractTypeDef name _ _)) = name
itemName (IAssociatedType (AssociatedTypeDef _ _ (ImplClause name lps tps) _)) =
    "<associated type: " ++ name ++ " " ++ show tps ++ ">"
itemName (IStatic (StaticDef name _ _)) = name
itemName (IMeta s) = "<metadata item: " ++ s ++ ">"
itemName (IUseDefault (UseDefault _ _ (ImplClause name _ tps))) = "<use default: " ++ name ++ " " ++ show tps ++ ">"
itemName (ITraitImpl (TraitImpl _ _ (ImplClause name _ tps) _)) = "<trait impl: " ++ name ++ " " ++ show tps ++ ">"
itemName (IDriver e) = "<driver>"


isDst ty = case ty of
    TStr -> True
    TVec _ -> True
    _ -> False

isFatPointer ty = case ty of
    TPtr _ t | isDst t -> True
    TRef _ _ t | isDst t -> True
    _ -> False


computedType ix (Expr ty e) = case e of
    EConst c ->
        let ConstDef _ ty' _ = i_consts ix M.! c
        in ty'
    ETupleLiteral es -> TTuple $ map (computedType ix) es
    EMatch _ (MatchArm _ e' : _) -> computedType ix e'
    EBlock _ e' -> computedType ix e'
    ECall f _ tas _ ->
        let fn = i_fns ix M.! f
            rawRet = fn_retTy fn
            tps = fn_tyParams fn
        in subst ([], tps) ([], tas) rawRet
    EUnsafe _ e' -> computedType ix e'
    EAssign _ _ -> TUnit
    EAssignOp _ _ _ -> TUnit
    EWhile _ _ -> TUnit
    EReturn _ -> TBottom
    EFor _ _ _ -> TUnit
    EBreak -> TBottom
    EContinue -> TBottom
    _ -> ty

