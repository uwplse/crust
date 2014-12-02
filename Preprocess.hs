{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad
import Data.Char (toLower)
import Data.Generics
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Text.Parsec hiding (label, State)

import Lexer
import Parser

import Debug.Trace

main = do
    items <- parseContents item
    let items' =
            constElim $
            ifFix $
            items
    putStrLn $ concatMap pp items'


constElim items = everywhere (mkT fixItem `extT` fixExpr) items
  where
    consts :: M.Map Name Expr_
    consts = everything M.union (M.empty `mkQ` collectItem) items
    collectItem (IConst (ConstDef name _ (Expr _ expr))) = M.singleton name expr
    collectItem _ = M.empty

    fixItem (IConst _ : xs) = xs
    fixItem xs = xs

    fixExpr (EConst n) = consts M.! n
    fixExpr e = e


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

mkCast e t = Expr t (ECast e t)

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
