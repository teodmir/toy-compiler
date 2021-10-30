{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypeChecker (runTypechecker, formatTypeError, formatTypeErrorNicer) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Parser
import SharedUtils

type VarEnv = M.Map Text Type

-- No longer 2 layers as in the evaluator
type BlockStack = [VarEnv]

data TCState = TCState
  { _getFunEnv :: FunEnv,
    _getBlocks :: BlockStack,
    _getCurrentFunInfo :: (Text, Type)
  }

initialState :: TCState
initialState =
  TCState
    { _getFunEnv = initialFunEnv,
      _getBlocks = mempty,
      _getCurrentFunInfo = undefined
    }

formatTypeError :: Err -> Text
formatTypeError (Err (Just (line, col)) msg) =
  "fail " <> T.pack (show line) <> " " <> T.pack (show col) <> "\n" <> msg
formatTypeError (Err Nothing msg) = msg

formatTypeErrorNicer :: Err -> Text
formatTypeErrorNicer (Err (Just (line, col)) msg) =
  "Error on line " <> T.pack (show line) <> ", column " <> T.pack (show col) <> ":\n" <> msg
formatTypeErrorNicer (Err Nothing msg) = msg

newtype TC a = TC
  {runTC :: ExceptT Err (State TCState) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError Err,
      MonadState TCState
    )

-- Find out if control reaches end of non-void function, returning the
-- position of the if statement that causes it.
-- This is done before the actual type checking.
data EndCheck = HasRet | NoRet | SingleRet Pos

-- If an if statement guarantees return, then we
-- require that either the else branch or the rest of the function has
-- a return statement that is guaranteed.
checkEndSingle :: Stmt -> EndCheck
checkEndSingle (Block stmts _) = checkEndBlock stmts
checkEndSingle s@(IfElse _ s1 s2 _) =
  case (checkEndSingle s1, checkEndSingle s2) of
    (r@(SingleRet _), _) -> r
    (_, r@(SingleRet _)) -> r -- Propagate error upwards
    (HasRet, NoRet) -> SingleRet $ stmtPos s
    (NoRet, HasRet) -> SingleRet $ stmtPos s
    (NoRet, NoRet) -> NoRet
    (HasRet, HasRet) -> HasRet
checkEndSingle s@(If _ s1 _) =
  case checkEndSingle s1 of
    HasRet -> SingleRet $ stmtPos s
    NoRet -> NoRet
    r@(SingleRet _) -> r
checkEndSingle (While _ stmt _) = checkEndSingle stmt
checkEndSingle Return {} = HasRet
checkEndSingle (ExprStmt _) = NoRet
checkEndSingle (Variable _) = NoRet

checkEndBlock :: [Stmt] -> EndCheck
checkEndBlock [] = NoRet
checkEndBlock [x] = checkEndSingle x
checkEndBlock (x@If {} : xs) =
  -- Immediate child - error salvageable if xs has ret
  case checkEndSingle x of
    r@(SingleRet _) -> case checkEndBlock xs of
      HasRet -> HasRet
      _ -> r
    _ -> checkEndBlock xs
checkEndBlock (x@IfElse {} : xs) =
  case checkEndSingle x of
    r@(SingleRet _) -> case checkEndBlock xs of
      HasRet -> HasRet
      _ -> r
    _ -> checkEndBlock xs
checkEndBlock (x : xs) =
  case checkEndSingle x of
    r@(SingleRet _) -> r
    _ -> checkEndBlock xs

checkEndGuard :: Text -> Function -> TC ()
checkEndGuard _ (Def Void _ _ _) = return ()
checkEndGuard _ Print = return ()
checkEndGuard name (Def t _ body pos) =
  case checkEndBlock body of
    HasRet -> return ()
    NoRet ->
      throwErr (Just pos) $
        "No return statement found in " <> name
          <> ", but has return type "
          <> T.pack (show t)
    SingleRet pos' -> throwErr (Just pos') "Potential end without return statement"

runTypechecker :: Program -> Either Err Program
runTypechecker prgm = evalState (runExceptT (runTC (typecheck prgm))) initialState

typecheck :: Program -> TC Program
typecheck prgm = do
  mapM_ addFunction prgm
  mapM_ (uncurry checkEndGuard) . M.toList <$> getFunEnv
  errGuardM
    (M.member "main" <$> getFunEnv)
    Nothing
    "No \"main\" function declared!"
  mapM tcDecl prgm

head' :: String -> [a] -> a
head' _ (x : _) = x
head' msg [] = error msg

tail' :: String -> [a] -> [a]
tail' _ (_ : xs) = xs
tail' msg [] = error msg

-- State utilities
-- Some of these are partial due to using head/tail, but if we somehow
-- find ourselves in a state where we attempt to access an empty
-- stack, then we are going to crash either way.

-- Current return type, based on the enclosing function (partial due
-- to inital state being undefined)
getCurrentFunInfo :: TC (Text, Type)
getCurrentFunInfo = gets _getCurrentFunInfo

putCurrentFunInfo :: Text -> Type -> TC ()
putCurrentFunInfo s t = do
  st <- get
  put $ st {_getCurrentFunInfo = (s, t)}

-- Blocks in current function
getBlocks :: TC BlockStack
getBlocks = gets _getBlocks

putBlocks :: BlockStack -> TC ()
putBlocks b = do
  st <- get
  put $ st {_getBlocks = b}

modifyBlocks :: (BlockStack -> BlockStack) -> TC ()
modifyBlocks f = putBlocks . f =<< getBlocks

popBlock :: TC ()
popBlock = modifyBlocks $ tail' "popBlock: empty block stack"

newBlock :: TC ()
newBlock = modifyBlocks (mempty :)

newVarEnv :: VarEnv -> TC ()
newVarEnv env = modifyBlocks (env :)

-- Get the "current" (head) environment of the stack
getVarEnv :: TC VarEnv
getVarEnv = head' "getVarEnv: empty block stack" <$> getBlocks

putVarEnv :: VarEnv -> TC ()
putVarEnv env = modifyBlocks $ (env :) . tail' "putVarEnv: empty block stack"

modifyVarEnv :: (VarEnv -> VarEnv) -> TC ()
modifyVarEnv f = putVarEnv . f =<< getVarEnv

lookupVar :: Text -> TC (Maybe Type)
lookupVar s = findFirst <$> getBlocks
  where
    findFirst [] = Nothing
    findFirst (x : xs) = M.lookup s x <|> findFirst xs

-- Is the variable declared in the current block scope?
isDeclaredHere :: Text -> TC Bool
isDeclaredHere s = M.member s <$> getVarEnv

-- Function environment utilities
getFunEnv :: TC FunEnv
getFunEnv = gets _getFunEnv

putFunEnv :: FunEnv -> TC ()
putFunEnv env = do
  st <- get
  put $ st {_getFunEnv = env}

lookupFun :: Text -> TC (Maybe Function)
lookupFun s = M.lookup s <$> getFunEnv

modifyFunEnv :: (FunEnv -> FunEnv) -> TC ()
modifyFunEnv f = putFunEnv . f =<< getFunEnv

funIsDeclared :: Text -> TC Bool
funIsDeclared name = M.member name <$> getFunEnv

-- Ensure e is of type t; return new expression
-- (possibly changed due to equality operation
-- information)
(-->) :: Expr -> Type -> TC Expr
e --> t = do
  (expr, t') <- tcExpr e
  when (t /= t') $
    throwErr (return $ exprPos e) $
      "Expected expression of type " <> T.pack (show t)
        <> ", but has type "
        <> T.pack (show t')
  return expr

-- Ensure e is NOT of type t; return t in the monad if it isn't
(-/->) :: Expr -> Type -> TC (Expr, Type)
e -/-> t = do
  (e', t') <- tcExpr e
  if t == t'
    then
      throwErr (return $ exprPos e) $
        "Expected expression not of type " <> T.pack (show t)
    else return (e', t')

tcArithOp :: Expr -> Bop -> Expr -> Pos -> TC (Expr, Type)
tcArithOp e1 op e2 pos = do
  e1' <- e1 --> Int
  e2' <- e2 --> Int
  return (Bop e1' op e2' pos undefined, Int)

tcCmpOp :: Expr -> Bop -> Expr -> Pos -> TC (Expr, Type)
tcCmpOp e1 op e2 pos = do
  e1' <- e1 --> Int
  e2' <- e2 --> Int
  return (Bop e1' op e2' pos undefined, Bool)

tcBoolOp :: Expr -> Bop -> Expr -> Pos -> TC (Expr, Type)
tcBoolOp e1 op e2 pos = do
  e1' <- e1 --> Bool
  e2' <- e2 --> Bool
  return (Bop e1' op e2' pos undefined, Bool)

tcUop :: Uop -> Expr -> Pos -> TC (Expr, Type)
tcUop Not e pos = do
  e' <- e --> Bool
  return (Uop Not e' pos, Bool)
tcUop Negation e pos = do
  e' <- e --> Int
  return (Uop Negation e' pos, Int)

tcIdentifier :: Text -> Pos -> TC Type
tcIdentifier name pos = do
  t <- lookupVar name
  case t of
    Just t' -> return t'
    Nothing ->
      throwErr (Just pos) $
        "use of undeclared identifier: " <> name

tcAsn :: Text -> Pos -> Expr -> TC (Expr, Type)
tcAsn name pos expr = do
  (expr', exprType) <- expr -/-> Void
  varType <- lookupVar name
  case varType of
    Just t -> do
      errGuard (t == exprType) (Just pos) $
        name <> " has type " <> T.pack (show t)
          <> ", but is assigned an expression of type "
          <> T.pack (show exprType)
      return (Assignment name expr' pos t, exprType)
    Nothing ->
      throwErr (Just pos) $
        "assignment to undeclared variable: " <> name

firstVoidIdx :: [Expr] -> TC (Maybe Int, [Expr])
firstVoidIdx exprs = do
  (exprs', ts) <- unzip <$> mapM tcExpr exprs
  let idx = (+ 1) <$> elemIndex Void ts
  return (idx, exprs')

tcPrint :: Pos -> [Expr] -> TC [Expr]
tcPrint pos exprs = do
  (i, exprs') <- firstVoidIdx exprs
  case i of
    Just _ ->
      throwErr (Just pos) $
        "call to print with void argument " <> "(" <> T.pack (show i) <> ")"
    Nothing -> return exprs'

checkArg :: VarDecl -> Expr -> TC Expr
checkArg (VarDecl _ t _) expr = do
  (expr', t') <- tcExpr expr
  errGuard (t' == t) (Just $ exprPos expr) $
    "type mismatch in function call: expected " <> T.pack (show t)
      <> ", called with "
      <> T.pack (show t')
  return expr'

checkArgs :: Text -> Pos -> FormalList -> [Expr] -> TC [Expr]
checkArgs name pos formals exprs = do
  let formalLen = length formals
  let exprsLen = length exprs
  errGuard (formalLen == exprsLen) (Just pos) $
    "Wrong number of arguments in call to " <> name <> ": "
      <> "expected "
      <> T.pack (show formalLen)
      <> ", called with "
      <> T.pack (show exprsLen)
  zipWithM checkArg formals exprs

tcFuncall :: Text -> Pos -> [Expr] -> TC (Expr, Type)
tcFuncall name pos exprs = do
  f <- lookupFun name
  case f of
    Just (Def t formals _ _) -> do
      exprs' <- checkArgs name pos formals exprs
      return (Funcall name exprs' pos t, t)
    Just Print -> do
      exprs' <- tcPrint pos exprs
      return (Funcall name exprs' pos Void, Void)
    Nothing -> throwErr (Just pos) $ "No such function: " <> name

tcEqExpr :: Bool -> Expr -> Expr -> Pos -> TC (Expr, Type)
tcEqExpr doNegate e1 e2 pos = do
  (e1', t1) <- tcExpr e1
  (e2', t2) <- tcExpr e2
  errGuard
    (t1 /= Void)
    (Just pos)
    "Expected non-void expression result"
  errGuard (t1 == t2) (Just pos) $
    "RHS of expression has type " <> T.pack (show t2)
      <> ", expected same type as LHS, namely "
      <> T.pack (show t1)
  return (Bop e1' (if doNegate then Neq else Eq) e2' pos t1, Bool)

tcExpr :: Expr -> TC (Expr, Type)
tcExpr e'@(IntExpr _ _) = return (e', Int)
tcExpr e'@(BoolExpr _ _) = return (e', Bool)
tcExpr (Identifier name pos _) = do
  t <- tcIdentifier name pos
  return (Identifier name pos t, t)
tcExpr (Bop e1 Eq e2 pos _) = tcEqExpr False e1 e2 pos
tcExpr (Bop e1 Neq e2 pos _) = tcEqExpr True e1 e2 pos
tcExpr (Bop e1 Add e2 pos _) = tcArithOp e1 Add e2 pos
tcExpr (Bop e1 Sub e2 pos _) = tcArithOp e1 Sub e2 pos
tcExpr (Bop e1 Mul e2 pos _) = tcArithOp e1 Mul e2 pos
tcExpr (Bop e1 Div e2 pos _) = tcArithOp e1 Div e2 pos
tcExpr (Bop e1 Lt e2 pos _) = tcCmpOp e1 Lt e2 pos
tcExpr (Bop e1 Gt e2 pos _) = tcCmpOp e1 Gt e2 pos
tcExpr (Bop e1 Lte e2 pos _) = tcCmpOp e1 Lte e2 pos
tcExpr (Bop e1 Gte e2 pos _) = tcCmpOp e1 Gte e2 pos
tcExpr (Bop e1 And e2 pos _) = tcBoolOp e1 And e2 pos
tcExpr (Bop e1 Or e2 pos _) = tcBoolOp e1 Or e2 pos
tcExpr (Uop op e pos) = tcUop op e pos
tcExpr (Assignment var e pos _) = tcAsn var pos e
tcExpr (Funcall name exprs pos _) = tcFuncall name pos exprs

tcBlock :: [Stmt] -> TC [Stmt]
tcBlock stmts = do
  newBlock
  stmts' <- mapM tcStmt stmts
  popBlock
  return stmts'

tcStmt :: Stmt -> TC Stmt
tcStmt (Block stmts pos) = liftM2 Block (tcBlock stmts) $ return pos
tcStmt (If expr stmt pos) = do
  expr' <- expr --> Bool
  stmt' <- tcStmt stmt
  return $ If expr' stmt' pos
tcStmt (IfElse expr s1 s2 pos) = do
  expr' <- expr --> Bool
  s1' <- tcStmt s1
  s2' <- tcStmt s2
  return $ IfElse expr' s1' s2' pos
tcStmt (While expr s pos) = do
  expr' <- expr --> Bool
  s' <- tcStmt s
  return $ While expr' s' pos
tcStmt (Return (Just expr) pos _) = do
  (name, ret) <- getCurrentFunInfo
  (expr', exprType) <- tcExpr expr
  errGuard (ret == exprType) (return $ exprPos expr) $
    "Return type of function " <> name <> " is " <> T.pack (show ret)
      <> ", but contains a return statement of type "
      <> T.pack (show exprType)
  return $ Return (Just expr') pos exprType
tcStmt s@(Return Nothing pos _) = do
  (name, ret) <- getCurrentFunInfo
  errGuard (ret == Void) (Just pos) $
    "Return type of function " <> name <> " is " <> T.pack (show ret)
      <> ", but contains void return statement"
  return s
tcStmt (ExprStmt e) = do
  (expr', _) <- tcExpr e
  return $ ExprStmt expr'
tcStmt s@(Variable (VarDecl name t pos)) = do
  errGuardM (not <$> isDeclaredHere name) (Just pos) $
    "multiple declarations of variable " <> name
  modifyVarEnv $ M.insert name t
  return s

addFunction :: Decl -> TC ()
addFunction f@(Decl _ name formals _ pos) = do
  guardDuplicateParams name formals
  errGuardM (not <$> funIsDeclared name) (return pos) $
    "Multiple definitions of function " <> name
  modifyFunEnv $ M.insert name $ declToFun f

tcDecl :: Decl -> TC Decl
tcDecl (Decl t name formals body pos) = do
  mapM_ checkParam formals
  let env = M.fromList $ map varAsPair formals
  putCurrentFunInfo name t
  newVarEnv env
  b <- tcBlock body
  popBlock
  return $ Decl t name formals b pos
  where
    checkParam (VarDecl name' t' pos') =
      errGuard (t' /= Void) (Just pos') $
        "expected non-void type of parameter " <> name'
          <> " in function "
          <> name
