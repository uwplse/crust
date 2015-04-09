{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleContexts #-}
module TreeShake
where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics hiding (typeOf)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Debug.Trace

import Builder (typeOf)
import Index
import Parser
import Pprint (ppTy, runPp)
import RefSeek
import Unify



collectDrivers :: [Item] -> [Driver]
collectDrivers = mapMaybe $ \i -> case i of IDriver d -> Just d; _ -> Nothing

collectUsed :: Index -> [Driver] -> [Item] -> (S.Set Name, S.Set Name)
collectUsed ix drivers items = execState (everything (>>) walk drivers) (S.empty, S.empty)
  where
    tyImpls :: M.Map Name [Ty]
    tyImpls = M.fromListWith (++) $ flip mapMaybe items $ \i -> case i of
        IAssociatedType (AssociatedTypeDef _ _ (ImplClause absName _ _) implTy) ->
            Just (absName, [implTy])
        _ -> Nothing

    fnImpls :: M.Map Name [Name]
    fnImpls = M.fromListWith (++) $ flip mapMaybe items $ \i -> case i of
        IFn (FnDef _ implName _ _ _ _ (Just (ImplClause absName _ _)) _ _) ->
            Just (absName, [implName])
        _ -> Nothing


    go :: (MonadState (S.Set Name, S.Set Name) m, Data d) => d -> m ()
    go = everything (>>) walk

    walk :: (MonadState (S.Set Name, S.Set Name) m, Data d) => d -> m ()
    walk = return () `mkQ` walkTy `extQ` walkExpr `extQ` walkPattern

    walkTy ty = case ty of
        TAdt name _ _ -> addTy name
        TAbstract name _ _ -> addAbstractTy name
        _ -> return ()

    walkExpr e = case e of
        EVar name -> addStatic name
        EConst name -> addConst name
        ECall name _ _ _ -> addFn name
        _ -> return ()

    walkPattern p = case p of
        PConst name -> addConst name
        _ -> return ()


    addTy name = recordTy name $
        let def = fromMaybe (error $ "no such ty: " ++ name) $ M.lookup name $ i_types ix
        in go def >> maybe (return ()) addFn (ty_dtor def)

    addAbstractTy name = recordTy name $
        mapM_ go (fromMaybe [] $ M.lookup name tyImpls)

    addStatic name = recordVal name $
        case M.lookup name $ i_statics ix of
            Just def -> go def
            Nothing -> return ()

    addConst name = recordVal name $
        go $ fromMaybe (error $ "no such const: " ++ name) $ M.lookup name $ i_consts ix

    addFn name = recordVal name $ do
        let def = fromMaybe (error $ "no such fn: " ++ name) $ M.lookup name $ i_fns ix
        go def
        case def of
            FAbstract _ -> mapM_ addFn (fromMaybe [] $ M.lookup name fnImpls)
            _ -> return ()


    recordTy name andThen = do
        (tys, vals) <- get
        if S.member name tys then return () else do
        traceShow ("add ty", name) $ do
        put (S.insert name tys, vals)
        andThen

    recordVal name andThen = do
        (tys, vals) <- get
        if S.member name vals then return () else do
        traceShow ("add val", name) $ do
        put (tys, S.insert name vals)
        andThen


filterUnused :: (S.Set Name, S.Set Name) -> [Item] -> [Item]
filterUnused (tyNames, valNames) = filter go
  where
    go (IStruct def) = ty_name (TStruct def) `S.member` tyNames
    go (IEnum def) = ty_name (TEnum def) `S.member` tyNames
    go (IConst (ConstDef name _ _)) = name `S.member` valNames
    go (IFn def) = fn_name (FConcrete def) `S.member` valNames
    go (IAbstractFn def) = fn_name (FAbstract def) `S.member` valNames
    go (IExternFn def) = fn_name (FExtern def) `S.member` valNames
    go (IAbstractType (AbstractTypeDef name _ _)) = name `S.member` tyNames
    go (IAssociatedType _) = True
    go (IStatic (StaticDef name _ _)) = name `S.member` valNames
    go (IUseDefault _) = True
    go (IDriver _) = True
    go (IMeta _) = True

shakeTree :: Index -> [Item] -> [Item]
shakeTree ix items = filterUnused names items
  where
    names = collectUsed ix drivers items
    drivers = collectDrivers items
