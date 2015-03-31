{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module DropGlue
where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics hiding (typeOf)
import qualified Data.Map as M
import qualified Data.Set as S

import Parser hiding (field)
import Index
import Builder

import Debug.Trace


generateDropGlues items =
        map (IFn . mkDropGlue ix . snd) (M.toList $ i_types ix) ++
        map applyDropGlue items
  where
    ix = mkIndex items

    applyDropGlue (IStruct (StructDef name a b c _)) =
        IStruct $ StructDef name a b c (Just $ dropGlueName name)
    applyDropGlue (IEnum (EnumDef name a b c _)) =
        IEnum $ EnumDef name a b c (Just $ dropGlueName name)
    applyDropGlue i = i

dropGlueName name = name ++ "$__drop_glue"

mkDropGlue ix ty = FnDef Private (dropGlueName $ ty_name ty) lifetimes tyParams [argDecl] TUnit Nothing [] body
  where
    lifetimes = ty_lifetimeParams ty
    tyParams = ty_tyParams ty
    tyArgs = map TVar tyParams
    argTy = TPtr MMut $ TAdt (ty_name ty) lifetimes tyArgs
    argDecl = ArgDecl $ Pattern argTy $ PVar "self"
    argUse = mkE ix $ var argTy "self"

    dtorCall = case ty_dtor ty of
        Just dtor -> [sexpr $ call dtor lifetimes tyArgs [return argUse]]
        Nothing -> []
    glueCalls = case ty of
        TStruct x -> structGlueCalls argTy
        TEnum x -> enumGlueCalls argTy

    body = mkE ix $ block (dtorCall ++ glueCalls) unit

structGlueCalls selfTy = (:[]) $ do
    self <- deref (var selfTy "self")
    let structName = adtName $ typeOf self

    StructDef _ _ _ fs _ <- getStruct structName

    calls <- forM fs $ \(FieldDef fieldName ty) -> do
        fld <- field self fieldName
        sexpr $ call "drop_glue" [] [typeOf fld] [addrOf MMut (return fld)]
    sexpr $ block (map return calls) unit

enumGlueCalls selfTy = (:[]) $ sexpr $ matchEnum (deref (var selfTy "self")) $
    \_ fs -> do
        calls <- forM fs $ \fld -> do
            sexpr $ call "drop_glue" [] [typeOf fld] [addrOf MMut (return fld)]
        block (map return calls) unit

collectRvalues = go
  where
    go :: Data d => d -> [Expr]
    go = (concat . gmapQ go) `extQ` goExpr

    goExpr e | isPath e = [e]
    goExpr (Expr _ (EAddrOf _)) = []
    goExpr (Expr _ (EAssign _ rhs)) = goExpr rhs
    goExpr e = concat $ gmapQ go e

    isPath (Expr _ e) = case e of
        EVar _ -> True
        EField e' _ -> isPath e'
        _ -> False

data CleanupState = CleanupState
    { cs_pending :: S.Set Name
    , cs_flagged :: S.Set Name
    , cs_scopes :: [[(Name, Ty)]]
    }

set_cs_pending x r = r { cs_pending = x }
set_cs_flagged x r = r { cs_flagged = x }
set_cs_scopes x r = r { cs_scopes = x }

update_cs_pending f r = r { cs_pending = f $ cs_pending r }
update_cs_flagged f r = r { cs_flagged = f $ cs_flagged r }
update_cs_scopes f r = r { cs_scopes = f $ cs_scopes r }

addPending n = modify $ update_cs_pending $ S.insert n
removePending n = modify $ update_cs_pending $ S.delete n

setFlagged n = do
    modify $ update_cs_pending $ S.delete n
    modify $ update_cs_flagged $ S.insert n

extendScope n ty = modify $ update_cs_scopes $ \(cur:rest) -> ((n, ty) : cur) : rest

pushScope = modify $ update_cs_scopes $ ([]:)

popScope = do
    x <- gets $ head . cs_scopes
    modify $ update_cs_scopes $ tail
    return x

withScope a = do
    pushScope
    x <- a
    s <- popScope
    return (x, s)

forkState f xs = do
    s <- get
    parts <- forM xs $ \x -> do
        put s
        y <- f x
        s' <- get
        return (y, s')
    put s
    return $ unzip parts

addCleanup :: Index -> [Item] -> [Item]
addCleanup ix xs = evalState (runReaderT (go xs) ix) (CleanupState S.empty S.empty [[]])
  where
    go :: (MonadReader Index m, MonadState CleanupState m, Data d) => d -> m d
    go = gmapM go `extM` goStmt `extM` goExpr

    walkPat = everywhereM (mkM go)
      where go p@(Pattern ty (PVar name)) = do
                extendScope name ty
                addPending name
                return p
            go p@(Pattern ty (PRefVar name)) = do
                extendScope name ty
                addPending name
                return p
            go p = return p

    goStmt (SLet pat expr) = do
    -- TODO: collect names and tys from pat
        expr' <- go expr
        walkPat pat
        return $ SLet pat expr'
    goStmt (SExpr expr) = do
        expr' <- go expr
        return $ SExpr expr'

    goExpr (Expr ty (EBlock ss e)) = do   
        (ss', e') <- goBlock ss e
        block ss' e'
    goExpr (Expr ty (EUnsafe ss e)) = do   
        (ss', e') <- goBlock ss e
        unsafe ss' e'
    goExpr (Expr ty (EAddrOf l)) = do
        l' <- goLval l
        return $ Expr ty $ EAddrOf l'
    goExpr (Expr ty (EAssign l r)) = do
        r' <- go r
        l' <- goLval l
        case l of
            Expr _ (EVar n) -> addPending n
            _ -> return ()
        return $ Expr ty $ EAssign l' r'
    goExpr (Expr ty (EVar n)) = do
        removePending n
        return $ Expr ty $ EVar n
    goExpr (Expr ty (EMatch e arms)) = do
        e' <- go e
        (arms', states) <- forkState go arms
        when (not $ null states) $ put $ foldl1 reconcile states
        return $ Expr ty $ EMatch e' arms'
    goExpr e = gmapM go e

    reconcile (CleanupState p1 f1 scopes) (CleanupState p2 f2 _) = CleanupState p' f' scopes
      where
        diff = S.difference p1 p2 `S.union` S.difference p2 p1
        f' = f1 `S.union` f2 `S.union` diff
        p' = p1 `S.difference` f'

    goLval e@(Expr ty (EVar n)) = return e
    goLval (Expr ty (EField e f)) = do
        e' <- goLval e
        return $ Expr ty $ EField e f
    goLval e = gmapM go e

    goBlock ss e = do
        ((ss', e'), locals) <- withScope $ do
            ss' <- go ss
            e' <- go e
            return (ss', e')

        pending <- gets cs_pending
        flagged <- gets cs_flagged
        let dropLocals = filter (flip S.member pending . fst) locals
            flagLocals = filter (flip S.member flagged . fst) locals

        flagDecls <- forM flagLocals $ \(name, ty) ->
            let_ ("__drop_" ++ name) false
        flagDrops <- forM flagLocals $ \(name, ty) ->
            sexpr $ if_ (var TBool ("__drop_" ++ name)) $
                call "drop_glue" [] [ty] [addrOf MMut $ var ty name]
        fixedDrops <- forM dropLocals $ \(name, ty) ->
            sexpr $ call "drop_glue" [] [ty] [addrOf MMut $ var ty name]

        let (ss'', e'') =
                if null flagLocals
                    then (ss', e')
                    else updateFlags (S.fromList $ map fst flagLocals) (ss', e')

            resultDecl = let_ "__result" (return e'') 
            stmts =
                map return flagDecls ++
                map return ss'' ++
                [resultDecl] ++
                map return flagDrops ++
                map return fixedDrops

        return (stmts, var (typeOf e'') "__result")

    updateFlags flagVars = id
        --if S.null flagVars then id else error "no support for drop flags yet"
