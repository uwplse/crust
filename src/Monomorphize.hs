{-# LANGUAGE NoMonomorphismRestriction #-}
module Monomorphize
where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Index
import Parser
import Pprint
import Unify

values = map snd . M.toList


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
  where doSubst = subst (lps, tps) (replicate (length lps) "r_mono", tys)

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
    }

type MonoM a = StateT MonoState (Reader MonoCtx) a

monoFn :: AnyFnDef -> [Ty] -> MonoM AnyFnDef
monoFn fd tys = populateFn (fn_name fd) $ do
    let fd' = case fd of
            FConcrete f -> FConcrete $ substFn f tys
            FAbstract _ -> error "unexpected FAbstract in monoFn'"
            FExtern f -> FExtern $ substExternFn f tys
    monoRefs fd'

monoFnName :: Name -> [Ty] -> MonoM Name
monoFnName name tys = do
    fd <- asks $ fromMaybe (error $ "mono: no such fn: " ++ name) .
        M.lookup name . i_fns . mc_ix
    fd' <- monoFn fd tys
    return $ fn_name fd'

monoType :: TypeDef -> [Ty] -> MonoM TypeDef
monoType td tys = populateType (ty_name td) $ do
    let td' = case td of
            TStruct s -> TStruct $ substStruct s tys
            TEnum e -> TEnum $ substEnum e tys
    monoRefs td'

monoTypeName :: Name -> [Ty] -> MonoM Name
monoTypeName name tys = do
    td <- asks $ fromMaybe (error $ "mono: no such type: " ++ name) .
        M.lookup name . i_types . mc_ix
    td' <- monoType td tys
    return $ ty_name td'


-- Abstract resolution

buildFnsByImpl :: Index -> M.Map Name [FnDef]
buildFnsByImpl ix = M.fromListWith (++) $ mapMaybe go $ values $ i_fns ix
  where
    go (FConcrete fd@(FnDef _ _ _ _ _ _ (Just (ImplClause name _ _)) _ _)) = Just (name, [fd])
    go _ = Nothing

resolveCall :: Name -> [Ty] -> MonoM Name
resolveCall absName tys = do
    allImpls <- asks $ fromMaybe [] . M.lookup absName . mc_fns_by_impl
    let goodImpls = mapMaybe (unifyImpl tys) allImpls
    case goodImpls of
        [(name, tys)] -> monoFnName name tys
        [] -> error $ "no instance for " ++ absName ++ show (map (runPp . ppTy) tys)
        _ -> error $ "multiple instances for " ++ absName ++ show (map (runPp . ppTy) tys) ++ ":" ++
            concatMap (\(name, ty) -> "\n" ++ name ++ show (map (runPp . ppTy) tys)) goodImpls

unifyImpl :: [Ty] -> FnDef -> Maybe (Name, [Ty])
unifyImpl tys (FnDef _ name _ tps _ _ (Just (ImplClause _ _ implTas)) _ _) = do
    let (uty, intern) = toUTy (TTuple implTas)
    result <- case unify uty (TTuple tys) of
        [x] -> Just x
        [] -> Nothing
        xs -> error "unexpected: multiple unify results"
    let tyArgs = flip map tps $ \param ->
            case M.lookup param intern of
                Just i -> getUnifiedArg result i
                Nothing -> TUnit
    return (name, tyArgs)


-- Full recursive monomorphization

-- TODO: struct/enum dtors
monoRefs x = walk x
  where
    walk :: Data d => d -> MonoM d
    walk = gmapM walk `extM` goExpr `extM` goTy

    goExpr (ECall name _ tys args) = do
        name' <- resolveCall name tys
        args' <- mapM walk args
        return $ ECall name' [] [] args'
    goExpr e = gmapM walk e

    goTy (TAdt name _ tys) = do
        name' <- monoTypeName name tys
        return $ TAdt name' [] []
    goTy t = gmapM walk t


populate getter updater name act = do
    fns <- gets getter
    case M.lookup name fns of
        Just f -> return f
        Nothing -> do
            f <- act
            modify (updater name f)
            return f

populateFn = populate ms_fns $ \k v s -> s { ms_fns = M.insert k v $ ms_fns s }

populateType = populate ms_types $ \k v s -> s { ms_types = M.insert k v $ ms_types s }



runMono :: Index -> MonoM a -> [Item]
runMono ix act = items
  where
    fnsByImpl = buildFnsByImpl ix
    ctx = MonoCtx ix fnsByImpl

    consts = i_consts ix
    statics = i_statics ix

    act' = do
        act
        consts' <- mapM monoRefs $ values consts
        statics' <- mapM monoRefs $ values statics
        return (consts', statics')

    act'' = runStateT act' $ MonoState M.empty M.empty
    ((consts', statics'), ms) = runReader act'' ctx

    items = reconstruct (values $ ms_fns ms) (values $ ms_types ms) consts' statics'

    reconstruct fns tys consts statics =
        map goFn fns ++ map goTy tys ++ map IConst consts ++ map IStatic statics
      where
        goFn (FConcrete f) = IFn f
        goFn (FAbstract _) = error "unexpected FAbstract in reconstruct"
        goFn (FExtern f) = IExternFn f

        goTy (TStruct s) = IStruct s
        goTy (TEnum e) = IEnum e

--monoTest ix is = runMono ix $ monoFnName "trait3$g" [TUint $ BitSize 32]
monoTest ix is = runMono ix $ monoFnName "trait3$g" [TAdt "trait3$S" [] []]

