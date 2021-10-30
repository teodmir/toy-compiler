{
{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Prelude hiding (lex)
import Data.Text (Text)
import qualified Data.Text as T
}

%wrapper "posn"
@integer = [0-9]+
@identifier = [a-zA-Z_][a-zA-Z0-9_]*
@comment = "//".*\n

tokens :-
  $white+ ;
  @comment ;

  if { lex $ TokenKeyw KeywIf }
  else { lex $ TokenKeyw KeywElse }
  while { lex $ TokenKeyw KeywWhile }
  return { lex $ TokenKeyw KeywReturn }
  int { lex $ TokenKeyw KeywInt }
  bool { lex $ TokenKeyw KeywBool }
  void { lex $ TokenKeyw KeywVoid }

  true { lex $ TokenBool Ttrue }
  false { lex $ TokenBool Tfalse }

  \( { lex $ TokenSep SepLpar }
  \) { lex $ TokenSep SepRpar }
  \{ { lex $ TokenSep SepLbrace }
  \} { lex $ TokenSep SepRbrace }
  \; { lex $ TokenSep SepSemi }
  \, { lex $ TokenSep SepComma }

  \= { lex $ TokenOp OpAsn }
  \|\| { lex $ TokenOp OpOr }
  \&& { lex $ TokenOp OpAnd }
  \== { lex $ TokenOp OpEq }
  \!= { lex $ TokenOp OpNeq }
  \< { lex $ TokenOp OpLt }
  \> { lex $ TokenOp OpGt }
  \<= { lex $ TokenOp OpLte }
  \>= { lex $ TokenOp OpGte }
  \+ { lex $ TokenOp OpPlus }
  \- { lex $ TokenOp OpMinus }
  \* { lex $ TokenOp OpMul }
  \/ { lex $ TokenOp OpDiv }
  \! { lex $ TokenOp OpNot }

  @identifier { lexStr (TokenId . T.pack) }

  @integer { lexStr (TokenInt . T.pack) }

{
type Pos = (Int, Int)

extractPosn (AlexPn _ line col) = (line, col)

-- Partially apply these to create lexing functions that fit the type
-- expected by the Alex posn wrapper (AlexPosn -> String -> a)
lex :: Token -> AlexPosn -> String -> (Token, Pos)
lex tok pos _ = (tok, extractPosn pos)

lexStr :: (String -> Token) -> AlexPosn -> String -> (Token, Pos)
lexStr f pos str = (f str, extractPosn pos)

data Keyw = KeywIf
          | KeywElse
          | KeywWhile
          | KeywReturn
          | KeywInt
          | KeywBool
          | KeywVoid
          deriving (Show)

data Sep = SepLpar
         | SepRpar
         | SepLbrace
         | SepRbrace
         | SepSemi
         | SepComma
         deriving (Show)

data Op = OpAsn
        | OpOr
        | OpAnd
        | OpEq
        | OpNeq
        | OpLt
        | OpGt
        | OpLte
        | OpGte
        | OpPlus
        | OpMinus
        | OpMul
        | OpDiv
        | OpNot
        deriving (Show)

data TBool = Ttrue | Tfalse
           deriving (Show)

data Token = TokenKeyw Keyw
           | TokenBool TBool
           | TokenSep Sep
           | TokenOp Op
           | TokenId Text
           | TokenInt Text
           deriving (Show)
}
