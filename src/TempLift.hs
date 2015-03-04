{-# LANGUAGE NoMonomorphismRestriction, Rank2Types #-}
module TempLift
where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics hiding (typeOf)

import Parser
import Index
import Builder

import Debug.Trace


hasStableLocation (Expr _ (EVar _)) = True
hasStableLocation (Expr _ (EField e _)) = hasStableLocation e
hasStableLocation (Expr _ (EDeref _)) = True
hasStableLocation _ = False

type Decl = (Name, Expr)

data LifterState = LifterState
    { ls_counter :: Int
    , ls_decls :: [Decl]
    }

set_ls_counter x r = r { ls_counter = x }
set_ls_decls x r = r { ls_decls = x }

saveDecls a = do
    oldDecls <- gets ls_decls
    modify $ set_ls_decls []
    x <- a
    newDecls <- gets ls_decls
    modify $ set_ls_decls oldDecls
    return (x, newDecls)


type LifterM a = StateT LifterState (Reader Index) a

runLifterM ix a =
    let a' = evalStateT a (LifterState 0 [])
        a'' = runReader a' ix
    in a''


fresh :: String -> LifterM Name
fresh base = do
    counter <- gets ls_counter
    modify $ set_ls_counter (counter + 1)
    return $ base ++ "_" ++ show counter

addDecl n e = do
    decls <- gets ls_decls
    modify $ set_ls_decls ((n,e) : decls)

takeDecls = do
    decls <- gets ls_decls
    modify $ set_ls_decls []
    return $ reverse decls

collectDecls :: [Decl] -> LifterM a -> LifterM (a, [Decl])
collectDecls ds x = do
    x' <- x
    ds' <- takeDecls
    return (x', ds ++ ds')

consumeDecls :: ([Decl] -> a -> LifterM a) -> ([Decl] -> a -> LifterM (a, [Decl]))
consumeDecls f ds x = do
    x' <- f ds x
    ds' <- takeDecls
    return (x', ds')

-- Walk bottom-up through a data structure.  At each node, apply a function to
-- transform the node and generate extra data to propagate to the next higher
-- node.
traverse :: forall m a.  Monad m =>
    -- Transform a term using extra data from its children.
    (forall d. Data d => a -> d -> m (d, a)) ->
    -- Combine multiple pieces of extra data into one.
    ([a] -> a) ->
    (forall d. Data d => d -> m (d, a))
traverse finalize collect x = do
    (x', as) <- runStateT (gmapM go x) []
    finalize (collect $ reverse as) x'
  where
    go y = do
        (y', a) <- lift $ traverse finalize collect y
        modify (a:)
        return y'

newtype Tr m a d = Tr { unTr :: a -> d -> m (d, a) }
extTr def ext = unTr ((Tr def) `ext0` (Tr ext))
mkTr = extTr (\a d -> return (d, a))

liftTempsM :: [Item] -> LifterM [Item]
liftTempsM x = goItems x
  where
    goItems [] = return []
    goItems (i : is) = do
        i' <- goItem i
        is' <- goItems is
        return (i' ++ is')

    goItem (IFn f) = do
        (f', ds) <- traverse go concat f
        return [IFn f']
    goItem (IStatic s) = do
        (s', ds) <- traverse go concat s
        ss <- forM ds $ \(name, expr@(Expr ty _)) -> do
            return (IStatic $ StaticDef name ty expr)
        return $ [IStatic s'] ++ ss
    goItem i = return [i]

    go = mkTr (consumeDecls goStmt) `extTr` goExpr

    applyDecls decls e = do
        case decls of
            [] -> return e
            _ -> block (map (\(n, e) -> let_ n (return e)) decls) (return e)

    goStmt :: [Decl] -> Stmt -> LifterM Stmt
    goStmt ds (SExpr e) = do
        if hasStableLocation e then
            sexpr $ applyDecls ds e
        else do
            n <- fresh "lifttemp"
            let_ n $ applyDecls ds e
    goStmt ds (SLet p (Just e)) = do
        letP p $ applyDecls ds e
    goStmt [] (SLet p Nothing) = do
        return $ SLet p Nothing

    goExpr :: [Decl] -> Expr -> LifterM (Expr, [Decl])
    goExpr ds orig@(Expr ty (EAddrOf e))
      | not $ hasStableLocation e = collectDecls ds $ do
        n <- fresh "lifttemp"
        addDecl n e
        return $ Expr ty $ EAddrOf $ Expr (typeOf e) $ EVar n
    {-
    goExpr ds (Expr _ (EField e _))
      | not $ hasStableLocation e = collectDecls ds $ do
        error $ "can't yet handle accessing a field of a temporary"
        -}
    goExpr ds (Expr ty (EBlock ss e)) = collectDecls [] $ do
        e' <- applyDecls ds e
        return $ Expr ty $ EBlock ss e'
    -- TODO: scrutinee of a match is sometimes a temporary that needs lifting
    goExpr ds e = collectDecls ds $ return e

liftTemps ix x = runLifterM ix $ liftTempsM x
