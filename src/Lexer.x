{
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Lexer (Token(..), PosToken, AlexPosn(..), alexScanTokens) where

import Data.Data

}

%wrapper "posn"

$digit = [0-9]
$word = [a-zA-Z0-9_]

tokens :-
 
$digit+         { out $ INT . read }
$word+          { out $ WORD }
.               ;
\n              ;

{

data Token = 
    WORD String
  | INT Int
  deriving (Show, Data, Typeable)

type PosToken = (AlexPosn, Token)

deriving instance Typeable AlexPosn
deriving instance Data AlexPosn

out f p s = (p, f s)
}

