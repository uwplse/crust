{-# LANGUAGE NoMonomorphismRestriction #-}
module Builder
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as M

import Parser
import Index


type Builder a = Reader Index a

mkE :: Index -> Builder a -> a
mkE ix x = runReader x ix

var t x = return $ Expr t $ EVar x

typeOf (Expr t _) = t

call name las tas args = do
    args <- sequence args
    ix <- ask
    fn <- getFn name
    let retTy = subst (fn_lifetimeParams fn, fn_tyParams fn) (las, tas) (fn_retTy fn)
    return $ Expr retTy $ ECall name las tas args

addrOf m e = do
    e <- e
    return $ Expr (TPtr m (typeOf e)) $ EAddrOf e

deref e = do
    e <- e
    let ptrTy = typeOf e
        ty = case ptrTy of
            TRef _ _ t -> t
            TPtr _ t -> t
            _ -> error $ "can't deref: " ++ show ptrTy
    return $ Expr ty $ EDeref e

field e f = do
    ix <- ask
    let structTy = typeOf e
        (structName, las, tas) = case structTy of
            TAdt structName las tas -> (structName, las, tas)
            _ -> error $ "can't take field of " ++ show structTy

    struct <- getStruct structName

    let lps = ty_lifetimeParams $ TStruct struct
        tps = ty_tyParams $ TStruct struct
        fieldDeclTy = getFieldTy struct f
        fieldTy = subst (lps, tps) (las, tas) fieldDeclTy

    return $ Expr fieldTy $ EField e f

let_ n e = do
    e <- e
    return $ SLet n (typeOf e) e

sexpr :: Monad m => m Expr -> m Stmt
sexpr e = do
    e <- e
    return $ SExpr e

block :: Monad m => [m Stmt] -> m Expr -> m Expr
block ss e = do
    ss <- sequence ss
    e <- e
    return $ Expr (typeOf e) $ EBlock ss e

unit = return $ Expr TUnit $ ESimpleLiteral "_unit"



