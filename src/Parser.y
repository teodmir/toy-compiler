{
{-# LANGUAGE OverloadedStrings #-}
module Parser (module Parser, Pos) where

import Lexer
import Data.Text (Text)
import qualified Data.Text as T
}

%name parse
%tokentype { (Token, Pos) }
%error { parseError }

%token
  if     { (TokenKeyw KeywIf, _) }
  else   { (TokenKeyw KeywElse, _) }
  while  { (TokenKeyw KeywWhile, _) }
  return { (TokenKeyw KeywReturn, _) }
  int    { (TokenKeyw KeywInt, _) }
  bool   { (TokenKeyw KeywBool, _) }
  void   { (TokenKeyw KeywVoid, _) }

  true  { (TokenBool Ttrue, _) }
  false { (TokenBool Tfalse, _) }

  '(' { (TokenSep SepLpar, _) }
  ')' { (TokenSep SepRpar, _) }
  '{' { (TokenSep SepLbrace, _) }
  '}' { (TokenSep SepRbrace, _) }
  ';' { (TokenSep SepSemi, _) }
  ',' { (TokenSep SepComma, _) }

  '='  { (TokenOp OpAsn, _) }
  '||' { (TokenOp OpOr, _) }
  '&&' { (TokenOp OpAnd, _) }
  '==' { (TokenOp OpEq, _) }
  '!=' { (TokenOp OpNeq, _) }
  '<'  { (TokenOp OpLt, _) }
  '>'  { (TokenOp OpGt, _) }
  '<=' { (TokenOp OpLte, _) }
  '>=' { (TokenOp OpGte, _) }
  '+'  { (TokenOp OpPlus, _) }
  '-'  { (TokenOp OpMinus, _) }
  '*'  { (TokenOp OpMul, _) }
  '/'  { (TokenOp OpDiv, _) }
  '!'  { (TokenOp OpNot, _) }

  id     { (TokenId _, _) }
  integer { (TokenInt _, _) }

%right '='
%left '||'
%left '&&'
%left '==' '!='
%left '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%left NEG '!'
%nonassoc '('
-- The minus sign has higher priority when used as negation, see below

%%

-- Consing instead of appending for efficiency, hence the need to reverse the
-- list afterwards.
Program : Program1 { reverse $1 }

Program1 : {- empty -} { [] }
         | Program1 Decl { $2 : $1 }

Decl : Type id '(' FormalList ')' '{' Block '}' { Decl $1 (getId $2) $4 $7 (posOf $2) }

FormalList : {- empty -} { [] }
           | NonemptyFormalList { reverse $1 }

NonemptyFormalList : Type id { [VarDecl (getId $2) $1 (posOf $2)] }
                   | NonemptyFormalList ',' Type id  { VarDecl (getId $4) $3 (posOf $4) : $1 }

Type : int { Int }
     | bool { Bool }
     | void { Void }

Block : Block1 { reverse $1 }

Block1 : {- empty -} { [] }
       | Block1 Stmt  { $2 : $1 }

-- 'undefined' used as placeholders for the type fields, which will later be
-- added in the type checking phase.
Stmt : '{' Block '}' { Block $2 (posOf $1) }
     | if '(' Expr ')' Stmt { If $3 $5 (posOf $1) }
     | if '(' Expr ')' Stmt else Stmt { IfElse $3 $5 $7 (posOf $1) }
     | while '(' Expr ')' Stmt { While $3 $5 (posOf $1) }
     | return Expr ';' { Return (Just $2) (posOf $1) undefined}
     | return ';' { Return Nothing (posOf $1) undefined }
     | Expr ';' { ExprStmt $1 }
     | Type id ';' { Variable $ VarDecl (getId $2) $1 (posOf $2) }

Expr : '!' Expr { Uop Not $2 (posOf $1) }
     | '-' Expr %prec NEG { Uop Negation $2 (posOf $1) }
     | Expr '+' Expr  { Bop $1 Add $3 (posOf $2) undefined }
     | Expr '-' Expr  { Bop $1 Sub $3 (posOf $2) undefined }
     | Expr '*' Expr  { Bop $1 Mul $3 (posOf $2) undefined }
     | Expr '/' Expr  { Bop $1 Div $3 (posOf $2) undefined }
     | Expr '||' Expr { Bop $1 Or  $3 (posOf $2) undefined }
     | Expr '&&' Expr { Bop $1 And $3 (posOf $2) undefined }
     | Expr '==' Expr { Bop $1 Eq  $3 (posOf $2) undefined }
     | Expr '!=' Expr { Bop $1 Neq $3 (posOf $2) undefined }
     | Expr '<' Expr  { Bop $1 Lt  $3 (posOf $2) undefined }
     | Expr '>' Expr  { Bop $1 Gt  $3 (posOf $2) undefined }
     | Expr '<=' Expr { Bop $1 Lte $3 (posOf $2) undefined }
     | Expr '>=' Expr { Bop $1 Gte $3 (posOf $2) undefined }
     | integer { IntExpr (getInt $1) (posOf $1) }
     | true { BoolExpr True (posOf $1) }
     | false { BoolExpr False (posOf $1) }
     | id '=' Expr { Assignment (getId $1) $3 (posOf $1) undefined }
     | id { Identifier (getId $1) (posOf $1) undefined }
     | id '(' ExprList ')' { Funcall (getId $1) $3 (posOf $1) undefined }
     | '(' Expr ')' { $2 }

ExprList : {- Empty -} { [] }
         | NonemptyExprList { reverse $1 }

NonemptyExprList : Expr { [$1] }
                 | NonemptyExprList ',' Expr { $3 : $1 }

{
parseProgram = parse . alexScanTokens

parseError :: [(Token, Pos)] -> a
parseError [] = error "Parser error: unexpected end of stream"
parseError ((tok, (line, column)):_) =
  error $ "Parser error at line " <> show line <> ", column " <> show column
          <> ": unexpected " <> show tok

posOf = snd

getId (TokenId x, _) = x

getInt (TokenInt x, _) = read $ T.unpack x

varPos (VarDecl _ _ pos) = pos

stmtPos (Block _ pos) = pos
stmtPos (If _ _ pos) = pos
stmtPos (IfElse _ _ _ pos) = pos
stmtPos (While _ _ pos) = pos
stmtPos (Return _ pos _) = pos
stmtPos (ExprStmt expr) = exprPos expr
stmtPos (Variable var) = varPos var

exprPos (IntExpr _ pos) = pos
exprPos (BoolExpr _ pos) = pos
exprPos (Bop _ _ _ pos _) = pos
exprPos (Assignment _ _ pos _) = pos
exprPos (Identifier _ pos _) = pos
exprPos (Uop _ _ pos) = pos
exprPos (Funcall _ _ pos _) = pos

varAsPair :: VarDecl -> (Text, Type)
varAsPair (VarDecl name t _) = (name, t)

-- AST
type Program = [Decl]

data Decl = Decl Type Text FormalList [Stmt] Pos
  deriving Show

-- Map of identifiers to types
type FormalList = [VarDecl]

data VarDecl = VarDecl Text Type Pos
  deriving Show

data Type = Int | Bool | Void
  deriving Eq

instance Show Type where
  show Int = "int"
  show Bool = "bool"
  show Void = "void"

data Stmt = Block [Stmt] Pos
          | If Expr Stmt Pos
          | IfElse Expr Stmt Stmt Pos
          | While Expr Stmt Pos
          | Return (Maybe Expr) Pos Type
          | ExprStmt Expr
          | Variable VarDecl
  deriving Show

data Bop = Add
         | Sub
         | Mul
         | Div
         | And
         | Or
         | Eq
         | Neq
         | Lt
         | Gt
         | Lte
         | Gte
  deriving (Show, Eq)

data Uop = Not
         | Negation
  deriving (Show, Eq)

data Expr = IntExpr Int Pos
          | BoolExpr Bool Pos
          | Bop Expr Bop Expr Pos Type
          | Assignment Text Expr Pos Type
          | Identifier Text Pos Type
          | Uop Uop Expr Pos
          | Funcall Text [Expr] Pos Type
  deriving Show
}
