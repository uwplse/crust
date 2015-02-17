{-# LANGUAGE NoMonomorphismRestriction, Rank2Types #-}
module Rename
where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics hiding (typeOf)
import qualified Data.Map as M
import Data.Maybe

import Parser
import Index
import Builder

import Debug.Trace


data RenamerState = RenamerState
    { rs_name_map :: M.Map Name Name
    , rs_used_names :: M.Map Name Int
    }

set_rs_name_map x r = r { rs_name_map = x }
set_rs_used_names x r = r { rs_used_names = x }

update_rs_name_map f r = r { rs_name_map = f $ rs_name_map r }
update_rs_used_names f r = r { rs_used_names = f $ rs_used_names r }

-- Find the first unused name out of `name`, `name_0`, `name_1`, ...
makeFresh name used =
    case M.lookup name used of
        Nothing ->
            let used' = M.insert name 0 used
            in (name, used')
        Just count ->
            -- `count` is the smallest suffix that isn't known to already be in
            -- use.  If `name_count` is not in use, use that.  Otherwise, keep
            -- incrementing `count` until we get one that works.  When finished,
            -- mark `name_count` as in use, and update the value for `name` with
            -- the new `count`.
            let (name', count') = go name count
                used' = M.insert name' 0 $ M.insert name count' $ used
            in (name', used')
  where
    go name count
      | isNothing $ M.lookup name' used = (name', count + 1)
      | otherwise = go name (count + 1)
      where name' = name ++ "_" ++ show count

bindName n = do
    used <- gets rs_used_names
    let (n', used') = makeFresh n used
    modify $ set_rs_used_names used'
    modify $ update_rs_name_map $ M.insert n n'
    return n'

getName n = fromMaybe (error $ "undeclared local: " ++ n) <$> gets (M.lookup n . rs_name_map)

withScope a = do
    oldNames <- gets $ rs_name_map
    x <- a
    modify $ set_rs_name_map oldNames
    return x

withEmptyScope a = do
    old <- get
    x <- a
    put old
    return x

initState ix = RenamerState nameMap usedNames
  where
    names = M.keys (i_consts ix) ++ M.keys (i_statics ix)
    nameMap = M.fromList $ zip names names
    usedNames = M.fromList $ zip names (repeat 0)

renameLocals ix x = evalState (go x) (initState ix)
  where
    go :: forall d. Data d => d -> State RenamerState d
    go = gmapM go `extM` goExpr `extM` goMatchArm `extM` goStmtList `extM` goFnDef

    goExpr (EVar n) = do
        n' <- getName n
        return (EVar n')
    goExpr e@(EBlock _ _) = withScope $ gmapM go e
    goExpr e@(EUnsafe _ _) = withScope $ gmapM go e
    goExpr e = gmapM go e

    goMatchArm (MatchArm pat body) = withScope $ do
        pat' <- walkPat pat
        MatchArm pat' <$> go body

    walkPat = everywhereM (mkM go)
      where go (PVar name) = PVar <$> bindName name
            go (PRefVar name) = PRefVar <$> bindName name
            go p = return p

    goStmtList (s@(SLet pat expr) : ss) = do
        expr' <- go expr
        -- The name goes out of scope when the enclosing block does.
        pat' <- walkPat pat
        (SLet pat' expr' :) <$> go ss
    goStmtList ss = gmapM go ss

    goFnDef (FnDef name lps tps args retTy implClause body) = withEmptyScope $ do
        args' <- mapM renameArg args
        body' <- go body
        return $ FnDef name lps tps args' retTy implClause body'

    renameArg (ArgDecl pat) = do
        pat' <- walkPat pat
        return $ ArgDecl pat'
