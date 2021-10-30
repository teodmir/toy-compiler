{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Prettyprinter (runPP) where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Parser

text :: Text -> Doc ann
text s = pretty (s :: Text)

indentLen :: Int
indentLen = 4

indent' :: Doc ann -> Doc ann
indent' = indent indentLen

data Assoc = L | R | NA
  deriving (Show, Eq)

-- A state for storing
data PrinterState = PrinterState
  { _getOpposite :: Bool,
    _getPrec :: Int
  }
  deriving (Show)

initState :: PrinterState
initState = PrinterState {_getOpposite = False, _getPrec = 0}

newtype Printer a = Printer {runPrinter :: Reader PrinterState a}
  deriving (Functor, Applicative, Monad, MonadReader PrinterState)

getOpposite :: Printer Bool
getOpposite = asks _getOpposite

getPrec :: Printer Int
getPrec = asks _getPrec

needParens :: Int -> Printer Bool
needParens prec = do
  outerPrec <- getPrec
  opposite <- getOpposite
  return $ prec < outerPrec || opposite && prec == outerPrec

exprAssoc :: Expr -> Assoc
exprAssoc (Bop _ op _ _ _) = assocOf op
exprAssoc (Uop op _ _) = assocOf op
exprAssoc _ = NA

runPP :: Program -> Text
runPP p =
  renderStrict $
    layoutPretty defaultLayoutOptions $
      runReader (runPrinter $ prettyProg p) initState

prettyProg :: [Decl] -> Printer (Doc ann)
prettyProg p = vsep <$> mapM prettyDecl p

prettyDecl :: Decl -> Printer (Doc ann)
prettyDecl (Decl t name params stmts _) = do
  params' <- prettyFormal params
  body <- prettyBlock stmts
  return $
    text (pp t)
      <+> text name
      <> fillSep [params', body]

prettyFormal :: [VarDecl] -> Printer (Doc ann)
prettyFormal xs = do
  xs' <- tupled <$> mapM (uncurry prettyTypeVar . varAsPair) xs
  return $ parens xs'

prettyTypeVar :: (Monad m, Pr a) => Text -> a -> m (Doc ann)
prettyTypeVar s t = return $ text (pp t) <+> text s

block :: Doc ann -> Doc ann
block x = text "{" <> fillSep [nest indentLen $ line <> x, text "}"]

prettyBlock :: [Stmt] -> Printer (Doc ann)
prettyBlock b = do
  b' <- vsep <$> mapM prettyStmt b
  return $ block b'

-- Blocks put their opening brace on the same line
blockSep :: Stmt -> Doc ann
blockSep (Block _ _) = text " "
blockSep _ = indent' line

prettyStmt :: Stmt -> Printer (Doc ann)
prettyStmt (Block stmts _) = prettyBlock stmts
prettyStmt (If expr stmt _) = do
  expr' <- prettyExpr expr
  stmt' <- prettyStmt stmt
  return $
    text "if" <+> parens expr'
      <> blockSep stmt
      <> stmt'
prettyStmt (IfElse expr stmt1 stmt2 _) = do
  expr' <- prettyExpr expr
  stmt1' <- prettyStmt stmt1
  stmt2' <- prettyStmt stmt2
  -- Continue else on same line?
  let elseSep = case (stmt1, stmt2) of
        (Block _ _, If {}) -> text " "
        (Block _ _, IfElse {}) -> text " "
        (Block _ _, Block _ _) -> text " "
        _ -> line
  let (<?+/>) = case stmt2 of
        Block _ _ -> (<+>)
        If {} -> (<+>)
        IfElse {} -> (<+>)
        _ -> (\x y -> indent' $ fillSep [x, y])
  return $
    (text "if" <+> parens expr' <> indent' (blockSep stmt1) <> stmt1')
      <> elseSep
      <> text "else" <?+/> stmt2'
prettyStmt (While expr stmt _) = do
  expr' <- prettyExpr expr
  stmt' <- prettyStmt stmt
  return $ text "while" <+> parens expr' <> blockSep stmt <> stmt'
prettyStmt (Return Nothing _ _) = return $ text "return" <> semi
prettyStmt (Return (Just expr) _ _) = do
  expr' <- prettyExpr expr
  return $ text "return" <+> expr' <> semi
prettyStmt (ExprStmt expr) = (<> semi) <$> prettyExpr expr
prettyStmt (Variable (VarDecl name t _)) = (<> semi) <$> prettyTypeVar name t

localPrinter :: MonadReader PrinterState m => (t -> m a) -> t -> Bool -> Int -> m a
localPrinter f x opp prec =
  local
    (const PrinterState {_getPrec = prec, _getOpposite = opp})
    (f x)

prettyExpr :: Expr -> Printer (Doc ann)
prettyExpr (IntExpr x _) = return $ pretty x
prettyExpr (BoolExpr n _) =
  return $ if n then text "true" else text "false"
prettyExpr (Bop expr1 op expr2 _ _) = do
  expr1' <- localPrinter prettyExpr expr1 (exprAssoc expr1 == R) (precOf op)
  expr2' <- localPrinter prettyExpr expr2 (exprAssoc expr2 == L) (precOf op)
  let op' = text $ pp op
  let doc = mconcat [expr1', softline, op', softline, expr2']
  useParens <- needParens $ precOf op
  return $ if useParens then parens doc else doc
prettyExpr (Assignment s expr _ _) = do
  expr' <- localPrinter prettyExpr expr (exprAssoc expr == L) 1
  let doc = text s <+> text "=" <+> expr'
  useParens <- needParens 1
  return $ if useParens then parens doc else doc
prettyExpr (Identifier s _ _) = return $ text s
prettyExpr (Uop op expr _) = do
  expr' <- localPrinter prettyExpr expr False (precOf op)
  let op' = text $ pp op
  let doc = op' <> expr'
  useParens <- needParens $ precOf op
  return $ if useParens then parens doc else doc
prettyExpr (Funcall s exprs _ _) = do
  exprs' <- tupled <$> mapM (\x -> localPrinter prettyExpr x False 0) exprs
  return $ text s <> parens exprs'

-- strip :: Text -> Text
-- strip = filter $ not . isSpace

-- (===) :: Text -> Text -> Bool
-- [] === [] = True
-- [] === (_ : _) = False
-- (_ : _) === [] = False
-- (x : xs) === (y : ys) =
--   if x == y
--     then xs === ys
--     else
--       error $
--         "Fail: " ++ show x ++ "/=" ++ show y ++ "\n\n"
--           ++ (show $ x : xs)
--           ++ "\n\n"
--           ++ (show $ y : ys)
--           ++ "\n"

-- ppTest1 :: Text -> (Bool, Text, Text)
-- ppTest1 prgm =
--   let s1 = strip prgm
--       s2 = strip $ runPP $ parseProgram prgm
--    in (s1 === s2, s1, s2)

-- ppTest2 :: Text -> (Bool, Text, Text)
-- ppTest2 prgm =
--   let s1 = runPP (parseProgram prgm)
--       s2 = runPP $ parseProgram $ runPP $ parseProgram $ prgm
--    in (s1 === s2, s1, s2)

-- ppTest :: Text -> Text
-- ppTest prgm = case ppTest1 prgm of
--   (True, _, _) ->
--     case ppTest2 prgm of
--       (True, _, _) -> "True"
--       (False, s1, s2) -> error $ "Test 2 failed:\n" ++ s1 ++ "\n\n" ++ s2
--   (False, s1, s2) -> error $ "Test 1 failed:\n" ++ s1 ++ "\n\n" ++ s2

class Pr a where
  pp :: a -> Text

instance Pr Type where
  pp = T.pack . show

instance Pr Uop where
  pp Not = "!"
  pp Negation = "-"

instance Pr Bop where
  pp Add = "+"
  pp Sub = "-"
  pp Mul = "*"
  pp Div = "/"
  pp And = "&&"
  pp Or = "||"
  pp Eq = "=="
  pp Neq = "!="
  pp Lt = "<"
  pp Gt = ">"
  pp Lte = "<="
  pp Gte = ">="

class Op a where
  precOf :: a -> Int
  assocOf :: a -> Assoc

instance Op Bop where
  precOf Or = 2
  precOf And = 3
  precOf Eq = 4
  precOf Neq = 4
  precOf Lt = 5
  precOf Gt = 5
  precOf Lte = 5
  precOf Gte = 5
  precOf Add = 6
  precOf Sub = 6
  precOf Mul = 7
  precOf Div = 7
  assocOf Or = L
  assocOf And = L
  assocOf Eq = L
  assocOf Neq = L
  assocOf Lt = L
  assocOf Gt = L
  assocOf Lte = L
  assocOf Gte = L
  assocOf Add = L
  assocOf Sub = L
  assocOf Mul = L
  assocOf Div = L

instance Op Uop where
  precOf _ = 8
  assocOf _ = NA
