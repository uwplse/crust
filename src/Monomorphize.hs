{-# LANGUAGE NoMonomorphismRestriction #-}
module Monomorphize
where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Set as S

import Index
import Parser
import Pprint
import Unify

import Debug.Trace

values = map snd . M.toList

dummyRegions = map $ const "r_mono"


-- Name mangling

mangle mode name tys = name ++ "__$m" ++ mode ++ show (length tys) ++ goMulti tys
  where goMulti tys = concatMap (\t -> '_' : go t) tys
        go ty = case ty of
            TVar _ -> error "unexpected TVar in mangle"
            TAdt name _ tys -> mangle "t" name tys
            TTuple tys -> "$t" ++ show (length tys) ++ goMulti tys
            TRef _ mutbl ty -> "$r" ++ goMut mutbl ++ "_" ++ go ty
            TPtr mutbl ty -> "$r" ++ goMut mutbl ++ "_" ++ go ty
            TStr -> "$s"
            TVec ty -> "$v_" ++ go ty
            TFixedVec n ty -> "$vf" ++ show n ++ "_" ++ go ty
            TInt size -> "$i" ++ goSize size
            TUint size -> "$u" ++ goSize size
            TFloat i -> "$f" ++ show i
            TBool -> "$b"
            TChar -> "$c"
            TFn -> "$fn"
            TUnit -> "$0"
            TBottom -> error "unexpecetd TBottom in mangle"
            TAbstract _ _ _ -> error "unexpected TAbstract in mangle"

        goMut MMut = "m"
        goMut MImm = "i"

        goSize (BitSize n) = show n
        goSize PtrSize = "ptr"



-- Basic substitution

mkSubstGo lps tps tys = everywhere (mkT doSubst)
  where doSubst = subst (lps, tps) (dummyRegions lps, tys)

substFn :: FnDef -> [Ty] -> FnDef
substFn (FnDef vis name lps tps args retTy impl preds body) tys =
    FnDef vis (mangle "f" name tys) [] [] (go args) (go retTy) (go impl) (go preds) (go body)
  where go = mkSubstGo lps tps tys

substExternFn :: ExternFnDef -> [Ty] -> ExternFnDef
substExternFn (ExternFnDef abi name lps tps args retTy) tys =
    ExternFnDef abi (mangle "f" name tys) [] [] (go args) (go retTy)
  where go = mkSubstGo lps tps tys

substStruct :: StructDef -> [Ty] -> StructDef
substStruct (StructDef name lps tps fields dtor) tys =
    StructDef (mangle "t" name tys) [] [] (go fields) dtor
  where go = mkSubstGo lps tps tys

substEnum :: EnumDef -> [Ty] -> EnumDef
substEnum (EnumDef name lps tps variants dtor) tys =
    EnumDef (mangle "t" name tys) [] [] (go variants) dtor
  where go = mkSubstGo lps tps tys


-- Single-item monomorphization

data MonoState = MonoState
    { ms_fns :: M.Map Name AnyFnDef
    , ms_types :: M.Map Name TypeDef
    }

data MonoCtx = MonoCtx
    { mc_ix :: Index
    , mc_fns_by_impl :: M.Map Name [FnDef]
    , mc_types_by_impl :: M.Map Name [AssociatedTypeDef]
    }

type MonoM a = StateT MonoState (Reader MonoCtx) a

monoFn :: AnyFnDef -> [Ty] -> MonoM Name
monoFn fd tys = populateFn mangledName $ do
    let fd' = case fd of
            FConcrete f -> FConcrete $ substFn f tys
            FAbstract _ -> error $ "unexpected FAbstract in monoFn:" ++ mangledName
            FExtern f -> FExtern $ substExternFn f tys
    when (mangledName /= fn_name fd') $
        error $ "mangled name mismatch: " ++ mangledName ++ " /= " ++ fn_name fd'
    monoRefs fd'
  where mangledName = mangle "f" (fn_name fd) tys

monoFnName :: Name -> [Ty] -> MonoM Name
monoFnName name tys = do
    fd <- asks $ fromMaybe (error $ "mono: no such fn: " ++ name) .
        M.lookup name . i_fns . mc_ix
    monoFn fd tys

monoType :: TypeDef -> [Ty] -> MonoM Name
monoType td tys = populateType mangledName $ do
    let td' = case td of
            TStruct s -> TStruct $ substStruct s tys
            TEnum e -> TEnum $ substEnum e tys
    when (mangledName /= ty_name td') $
        error $ "mangled name mismatch: " ++ mangledName ++ " /= " ++ ty_name td'
    monoRefs =<< fixDtor td'
  where
    mangledName = mangle "t" (ty_name td) tys

    -- Take advantage of the fact that generateDropGlues makes every drop glue
    -- take exactly the same parameters as the type itself.
    fixDtor (TStruct (StructDef name [] [] fs (Just dtorName))) = do
        dtorName' <- monoFnName dtorName tys
        return $ TStruct $ StructDef name [] [] fs (Just dtorName')
    fixDtor (TEnum (EnumDef name [] [] vs (Just dtorName))) = do
        dtorName' <- monoFnName dtorName tys
        return $ TEnum $ EnumDef name [] [] vs (Just dtorName')
    fixDtor x = return x

monoTypeName :: Name -> [Ty] -> MonoM Name
monoTypeName name tys = do
    td <- asks $ fromMaybe (error $ "mono: no such type: " ++ name) .
        M.lookup name . i_types . mc_ix
    monoType td tys


-- Abstract resolution

buildFnsByImpl :: Index -> M.Map Name [FnDef]
buildFnsByImpl ix = M.fromListWith (++) $ mapMaybe go $ values $ i_fns ix
  where
    go (FConcrete fd@(FnDef _ _ _ _ _ _ (Just (ImplClause name _ _)) _ _)) = Just (name, [fd])
    go _ = Nothing

resolveCall :: Name -> [Ty] -> MonoM Name
resolveCall absName tys = do
    fnsByImpl <- asks mc_fns_by_impl
    if not $ M.member absName fnsByImpl then monoFnName absName tys else do
    let goodImpls = mapMaybe (unifyImplFn tys) $ fnsByImpl M.! absName
    case takeMostSpecific goodImpls of
        [(name, tys)] -> monoFnName name tys
        [] -> error $ "no instance for " ++ absName ++ show (map (runPp . ppTy) tys)
        _ -> error $
            "multiple instances for " ++ absName ++ show (map (runPp . ppTy) tys) ++ ":" ++
            concatMap (\(name, tys) -> "\n" ++ name ++ show (map (runPp . ppTy) tys)) goodImpls

typeSize ty = case ty of
    TVar _ -> 1
    TAdt _ _ tys -> 1 + maximum (map typeSize tys)
    TTuple tys -> 1 + maximum (map typeSize tys)
    TRef _ _ ty -> 1 + typeSize ty
    TPtr _ ty -> 1 + typeSize ty
    TStr -> 1
    TVec ty -> 1 + typeSize ty
    TFixedVec _ ty -> 1 + typeSize ty
    TInt _ -> 1
    TUint _ -> 1
    TFloat _ -> 1
    TBool -> 1
    TChar -> 1
    TFn -> 1
    TUnit -> 1
    TBottom -> 1
    TAbstract _ _ tys -> 1 + maximum (map typeSize tys)

takeMostSpecific :: [(Name, [Ty])] -> [(Name, [Ty])]
takeMostSpecific impls = takeWhile (\i -> implCost i == implCost (head sorted)) sorted
  where implCost = sum . map typeSize . snd
        sorted = sortBy (comparing implCost) impls


buildTypesByImpl :: [Item] -> M.Map Name [AssociatedTypeDef]
buildTypesByImpl items = M.fromListWith (++) $ mapMaybe go items
  where
    go (IAssociatedType td@(AssociatedTypeDef _ _ (ImplClause name _ _) _)) = Just (name, [td])
    go _ = Nothing

resolveAbstractType :: Ty -> MonoM Ty
resolveAbstractType (TAbstract absName _ tys) = do
    allImpls <- asks $ fromMaybe [] . M.lookup absName . mc_types_by_impl
    let goodImpls = mapMaybe (unifyImplType tys) allImpls
    case goodImpls of
        -- Abstract type impl may itself include abstract types, so operate
        -- recursively.
        [ty] -> everywhereM (mkM resolveAbstractType) ty
        [] -> error $ "no instance for " ++ absName ++ show (map (runPp . ppTy) tys)
        _ -> error $
            "multiple instances for " ++ absName ++ show (map (runPp . ppTy) tys) ++ ":" ++
            concatMap (\ty -> "\n" ++ runPp (ppTy ty)) goodImpls
resolveAbstractType ty = return ty


unifyImplFn :: [Ty] -> FnDef -> Maybe (Name, [Ty])
unifyImplFn tys (FnDef _ name _ tps _ _ (Just impl) _ _) = do
    tyArgs <- unifyImpl tps impl tys
    return (name, tyArgs)

unifyImplType :: [Ty] -> AssociatedTypeDef -> Maybe Ty
unifyImplType tys (AssociatedTypeDef lps tps impl concreteTy) = do
    tyArgs <- unifyImpl tps impl tys
    return $ subst (lps, tps) (dummyRegions lps, tyArgs) concreteTy

unifyImpl :: [TyParam] -> ImplClause -> [Ty] -> Maybe [Ty]
unifyImpl tps (ImplClause _ _ implTas) tys = do
    let (uty, intern) = toUTy (TTuple implTas)
    result <- case unify uty (TTuple tys) of
        [x] -> Just x
        [] -> Nothing
        xs -> error "unexpected: multiple unify results"
    return $ flip map tps $ \param ->
            case M.lookup param intern of
                Just i -> getUnifiedArg result i
                Nothing -> TUnit





-- Full recursive monomorphization

-- TODO: struct/enum dtors
monoRefs :: Data d => d -> MonoM d
monoRefs x = resolveTypes x >>= walk
  where
    walk :: Data d => d -> MonoM d
    walk = gmapM walk `extM` goExpr `extM` goTy

    -- TODO: hack to pass off drop_glue handling to crust.native
    goExpr e@(ECall "drop_glue" _ _ _) = gmapM walk e
    goExpr e@(ECall name _ tys args) = do
        exists <- checkFnExists name
        if not exists then
            return e
        else do
            name' <- resolveCall name tys
            args' <- mapM walk args
            return $ ECall name' [] [] args'
    goExpr e = gmapM walk e

    checkFnExists name = do
        concrete <- asks $ M.member name . i_fns . mc_ix
        abstract <- asks $ M.member name . mc_fns_by_impl
        return $ concrete || abstract

    goTy (TAdt name _ tys) = do
        name' <- monoTypeName name tys
        return $ TAdt name' [] []
    goTy t = gmapM walk t


    resolveTypes = everywhereM (mkM resolveAbstractType)


dummy name = error $ "recursive dependency on " ++ name

populateFn :: Name -> MonoM AnyFnDef -> MonoM Name
populateFn name mk = do
    fns <- gets ms_fns
    when (not $ M.member name fns) $ do
        modify $ \s -> s { ms_fns = M.insert name (dummy name) $ ms_fns s }
        f <- mk
        modify $ \s -> s { ms_fns = M.insert name f $ ms_fns s }
    return name

populateType :: Name -> MonoM TypeDef -> MonoM Name
populateType name mk = do
    types <- gets ms_types
    when (not $ M.member name types) $ do
        modify $ \s -> s { ms_types = M.insert name (dummy name) $ ms_types s }
        t <- mk
        modify $ \s -> s { ms_types = M.insert name t $ ms_types s }
    return name



runMono :: Index -> [Item] -> MonoM a -> [Item]
runMono ix items act = items'
  where
    fnsByImpl = buildFnsByImpl ix
    typesByImpl = buildTypesByImpl items
    ctx = MonoCtx ix fnsByImpl typesByImpl

    consts = i_consts ix
    statics = i_statics ix

    act' = do
        act
        consts' <- mapM monoRefs $ values consts
        statics' <- mapM monoRefs $ values statics
        return (consts', statics')

    act'' = runStateT act' $ MonoState M.empty M.empty
    ((consts', statics'), ms) = runReader act'' ctx

    items' = reconstruct (values $ ms_fns ms) (values $ ms_types ms) consts' statics'

    reconstruct fns tys consts statics =
        map goFn fns ++ map goTy tys ++ map IConst consts ++ map IStatic statics
      where
        goFn (FConcrete f) = IFn f
        goFn (FAbstract _) = error "unexpected FAbstract in reconstruct"
        goFn (FExtern f) = IExternFn f

        goTy (TStruct s) = IStruct s
        goTy (TEnum e) = IEnum e


-- Monomorphize recursively starting from the named (non-polymorphic)
-- functions.
monomorphize :: Index -> [Name] -> [Item] -> [Item]
monomorphize ix roots items = runMono ix items $ mapM (\n -> monoFnName n []) roots
