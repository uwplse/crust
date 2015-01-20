{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad
import Data.Char (toLower)
import Data.Generics
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec hiding (label, State)

import Lexer
import Parser
import Index
import TempLift
import Rename
import Pprint
import DropGlue

import Debug.Trace

main = do
    items <- parseContents item
    let ix = mkIndex items
    let items' =
            addCleanup ix $
            renameLocals $
            liftTemps ix $
            constElim $
            ifFix $
            fixAbort $
            fixBottom $
            fixAddress $
            items
    trace (runPp $ mapM_ ppItem items') $ return ()
    putStrLn $ concatMap pp items'

fixAddress = everywhere (mkT stripAddr)
  where
    stripAddr (Expr _ (EAddrOf (Expr _ (EDeref e)))) = e
    stripAddr (Expr _ (EAddrOf (Expr _ (EUnOp "UnDeref" e)))) = e
    stripAddr (Expr _ (EUnOp "UnDeref" (Expr _ (EAddrOf e)))) = e
    stripAddr (Expr _ (EDeref (Expr _ (EAddrOf e)))) = e
    stripAddr e = e

constElim items = filter (not . isConst) $ everywhere (mkT fixExpr `extT` fixPat) items
  where
    consts :: M.Map Name Expr_
    consts = everything M.union (M.empty `mkQ` collectItem) items
    collectItem (IConst (ConstDef name _ (Expr _ expr))) = M.singleton name (cleanup expr)
    collectItem _ = M.empty

    cleanup (EUnOp "UnNeg" (Expr _ (ESimpleLiteral s)))
      | head s == '-' = ESimpleLiteral $ tail s
      | otherwise = ESimpleLiteral ('-' : s)
    cleanup e = e

    isConst (IConst _) = True
    isConst _ = False

    fixExpr (EConst n) = consts M.! n
    fixExpr e = e

    fixPat (PConst n) = exprToPat $ consts M.! n
    fixPat e = e


ifFix = everywhere (mkT fixIf)
  where
    fixIf
        (EMatch e
                [MatchArm (Pattern TBool (PSimpleLiteral "true")) e1,
                 MatchArm (Pattern TBool (PSimpleLiteral "false")) e2]) =
         EMatch (mkCast e (TInt 32))
                [MatchArm (Pattern (TInt 32) (PSimpleLiteral "0")) e2,
                 MatchArm (Pattern (TInt 32) (PWild)) e1]
    fixIf x = x

fixAbort = everywhere (mkT go)
  where
    go (Expr _ (ECall "core_intrinsics_abort" _ _ _)) = Expr TUnit (ESimpleLiteral "_")
    go x = x

fixBottom = everywhere (mkT go)
  where
    go TBottom = TUnit
    go t = t


mkCast e t = Expr t (ECast e t)

exprToPat (ESimpleLiteral s) = PSimpleLiteral s
exprToPat e = error $ "exprToPat: can't convert: " ++ show e


parseContents p = parseInput p <$> getContents

parseInput p = parseInput' p "<input>"

parseInput' p filename text =
    let tokens = alexScanTokens text
        items = case parse (do x <- many p; eof; return x) filename tokens of
            Left err -> error $ show err
            Right x -> x
    in items

parseFile p f = do
    result <- parseInput' p f <$> readFile f
    evaluate result
