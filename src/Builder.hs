{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
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

addrOf' m e = do
    e <- e
    return $ Expr (TRef "r_addrof" m (typeOf e)) $ EAddrOf e

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

decl n ty = do
    return $ SDecl n ty

sexpr :: Monad m => m Expr -> m Stmt
sexpr e = do
    e <- e
    return $ SExpr e

block :: Monad m => [m Stmt] -> m Expr -> m Expr
block ss e = do
    ss <- sequence ss
    e <- e
    return $ Expr (typeOf e) $ EBlock ss e

unsafe ss e = do
    ss <- sequence ss
    e <- e
    return $ Expr (typeOf e) $ EUnsafe ss e

unit = return $ Expr TUnit $ ESimpleLiteral "_unit"

matchEnum :: MonadReader Index m => m Expr -> (Int -> [Expr] -> m Expr) -> m Expr
matchEnum e mkArm = do
    e <- e
    let ty = typeOf e
    EnumDef _ _ _ variants _ <- getEnum $ adtName ty

    arms <- forM (zip [0..] variants) $ \(idx, VariantDef name tys) -> do
        let varNames = take (length tys) $ map (\i -> "f" ++ show i) [0..]
            varPatterns = zipWith (\name ty -> Pattern ty $ PVar name) varNames tys
            varExprs = zipWith (\name ty -> Expr ty $ EVar name) varNames tys
            pattern = Pattern (typeOf e) $ PEnum name idx varPatterns
        body <- mkArm idx varExprs
        return $ MatchArm pattern body
    let (MatchArm _ (Expr matchTy _) : _) = arms
    return $ Expr matchTy $ EMatch e arms

true = return $ Expr TBool $ ESimpleLiteral "true"
false = return $ Expr TBool $ ESimpleLiteral "false"

assign l r = do
    l <- l
    r <- r
    return $ Expr TUnit $ EAssign l r

if_ c t = do
    c <- c
    t <- t
    unit <- unit
    return $ Expr TUnit $ EMatch c $
        [ MatchArm (Pattern TBool $ PSimpleLiteral "0") unit
        , MatchArm (Pattern TBool PWild) t
        ]

binop op a b = do
    a <- a
    b <- b
    let ty = if opIsCompare op then TBool else typeOf b
    return $ Expr ty $ EBinOp op a b

opIsCompare op = case op of
    "BiEq" -> True
    "BiLt" -> True
    "BiLe" -> True
    "BiNe" -> True
    "BiGe" -> True
    "BiGt" -> True
    _ -> False
