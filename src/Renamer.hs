{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Used only before code generation; didn't want to bother with
-- displaying the name properly in the evaluator and type checker

-- This relies on the fact that a program is well-typed
module Renamer (runRename) where

import Control.Applicative ((<|>))
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Parser

type UsesMap = M.Map Text Int

type NameMap = M.Map Text Text

type Stack = [NameMap]

initState :: (UsesMap, Stack)
initState = (mempty, [])

newtype R a = R {runR :: State (UsesMap, Stack) a}
  deriving (Functor, Applicative, Monad, MonadState (UsesMap, Stack))

getUsesMap :: R UsesMap
getUsesMap = gets fst

modifyUsesMap :: (UsesMap -> UsesMap) -> R ()
modifyUsesMap f = get >>= (\(m, s) -> put (f m, s))

getStack :: R Stack
getStack = gets snd

modifyStack :: (Stack -> Stack) -> R ()
modifyStack f = get >>= (\(m, s) -> put (m, f s))

modifyNameMap :: (NameMap -> NameMap) -> R ()
modifyNameMap f = do
  newMap <- f . head <$> getStack
  modifyStack (newMap :)

getCount :: Text -> R (Maybe Int)
getCount s = M.lookup s <$> getUsesMap

incCount :: Text -> R ()
incCount s =
  getCount s
    >>= \case
      Just x -> modifyUsesMap $ M.insert s (x + 1)
      Nothing -> modifyUsesMap $ M.insert s 1

renameVar :: Text -> R Text
renameVar s =
  getCount s
    >>= \case
      Just x -> return $ s <> "_" <> T.pack (show x)
      Nothing -> return $ s <> "_0"

insertVar :: Text -> Text -> R ()
insertVar s1 s2 = modifyNameMap $ M.insert s1 s2

newBlock :: R ()
newBlock = modifyStack (mempty :)

popBlock :: R ()
popBlock = modifyStack tail

runRename :: Program -> Program
runRename p = evalState (runR $ renamePrgm p) initState

resetMap :: R ()
resetMap = modifyStack (const mempty) >> modifyNameMap (const mempty)

renamePrgm :: Program -> R Program
renamePrgm = mapM (\d -> do d' <- renameDecl d; resetMap; return d')

renameDecl :: Decl -> R Decl
renameDecl (Decl t name formals body p) = do
  body' <- renameBlock body
  return $ Decl t name formals body' p

renameVarDecl :: VarDecl -> R VarDecl
renameVarDecl (VarDecl name t pos) = do
  newName <- renameVar name
  insertVar name newName
  incCount name
  return $ VarDecl newName t pos

renameBlock :: [Stmt] -> R [Stmt]
renameBlock b = do
  newBlock
  b' <- mapM renameStmt b
  popBlock
  return b'

lookupVar :: Text -> R (Maybe Text)
lookupVar s = lookupVar' s <$> getStack
  where
    lookupVar' _ [] = Nothing
    lookupVar' s' (x : xs) = M.lookup s' x <|> lookupVar' s' xs

renameStmt :: Stmt -> R Stmt
renameStmt (Block b pos) = do b' <- renameBlock b; return $ Block b' pos
renameStmt (If e s pos) = do
  e' <- renameExpr e
  s' <- renameStmt s
  return $ If e' s' pos
renameStmt (IfElse e s1 s2 pos) = do
  e' <- renameExpr e
  s1' <- renameStmt s1
  s2' <- renameStmt s2
  return $ IfElse e' s1' s2' pos
renameStmt (While e s pos) = do
  e' <- renameExpr e
  s' <- renameStmt s
  return $ While e' s' pos
renameStmt (Return e pos t) =
  liftM3 Return (sequence $ renameExpr <$> e) (return pos) (return t)
renameStmt (ExprStmt e) = ExprStmt <$> renameExpr e
renameStmt (Variable v) = Variable <$> renameVarDecl v

renameExpr :: Expr -> R Expr
renameExpr e@(IntExpr _ _) = return e
renameExpr e@(BoolExpr _ _) = return e
renameExpr orig@(Identifier s pos t) =
  lookupVar s
    >>= \case
      Just s' -> return $ Identifier s' pos t
      Nothing -> return orig
renameExpr (Bop e1 op e2 pos t) = do
  e1' <- renameExpr e1
  e2' <- renameExpr e2
  return $ Bop e1' op e2' pos t
renameExpr (Uop op e pos) = do
  e' <- renameExpr e
  return $ Uop op e' pos
renameExpr (Assignment s e pos t) = do
  e' <- renameExpr e
  lookupVar s >>= \case
    Just s' -> return $ Assignment s' e' pos t
    Nothing -> return $ Assignment s e' pos t
renameExpr (Funcall s es pos t) = do
  es' <- mapM renameExpr es
  return $ Funcall s es' pos t
