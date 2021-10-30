{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedUtils where

import Control.Monad.Except
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Parser

data Err = Err (Maybe Pos) Text

declToFun :: Decl -> Function
declToFun (Decl t _ formals body pos) = Def t formals body pos

data Function
  = Def Type FormalList [Stmt] Pos
  | Print
  deriving (Show)

type FunEnv = M.Map Text Function

initialFunEnv :: M.Map Text Function
initialFunEnv = M.fromList [("print", Print)]

throwErr :: (MonadError Err m) => Maybe Pos -> Text -> m a
throwErr pos s = throwError $ Err pos s

errGuardM :: (MonadError Err m) => m Bool -> Maybe Pos -> Text -> m ()
errGuardM b mpos s = do b' <- b; if b' then return () else throwErr mpos s

errGuard :: (MonadError Err m) => Bool -> Maybe Pos -> Text -> m ()
errGuard b = errGuardM $ return b

varName :: VarDecl -> Text
varName (VarDecl name _ _) = name

firstDupe :: FormalList -> Maybe VarDecl
firstDupe [] = Nothing
firstDupe (x : xs) =
  if isJust $ find (nameEq x) xs
    then Just x
    else firstDupe xs
  where
    nameEq x' y = varName x' == varName y

guardDuplicateParams :: (MonadError Err m) => Text -> FormalList -> m ()
guardDuplicateParams funName formals =
  case firstDupe formals of
    Just (VarDecl name _ pos) ->
      throwErr (Just pos) $
        "Reused parameter name " <> T.pack (show name)
          <> " in function "
          <> funName
    Nothing -> return ()
