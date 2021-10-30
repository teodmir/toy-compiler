{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator (runEvaluator, formatEvalError) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Class
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import SharedUtils

data Value = IntVal Int | BoolVal Bool

data Ret = Cont | Val (Maybe Value)

instance Show Value where
  show (IntVal x) = show x
  show (BoolVal b) = show b

builtinPrint :: [Value] -> Eval ()
builtinPrint = liftIO . TIO.putStrLn . T.intercalate " " . fmap (T.pack . show)

type VarEnv = M.Map Text Value

-- 2 layers: Stack of stacks of environments
type BlockStack = [VarEnv]

type CallStack = [BlockStack]

data EvalState = EvalState {_getFunEnv :: FunEnv, _getCallStack :: CallStack}

initialState :: EvalState
initialState = EvalState {_getFunEnv = initialFunEnv, _getCallStack = mempty}

newtype Eval a = Eval
  {runEval :: ExceptT Err (StateT EvalState IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError Err,
      MonadState EvalState,
      MonadIO
    )

runEvaluator :: Program -> IO (Either Err ())
runEvaluator prgm = do
  evalStateT (runExceptT (runEval (eval prgm))) initialState

eval :: Program -> Eval ()
eval prgm = do
  mapM_ evalDecl prgm
  maybeMain <- M.lookup "main" <$> getFunEnv
  case maybeMain of
    Just (Def _ _ body _) -> pushEmptyFunEnv >> evalBlock body >> popCallStack
    _ -> throwErr Nothing "No \"main\" function declared!"

formatEvalError :: Err -> Text
formatEvalError (Err (Just (line, col)) msg) =
  "INTERPRETATION ERROR: " <> "at line " <> T.pack (show line)
    <> ", column "
    <> T.pack (show col)
    <> ":\n"
    <> msg
formatEvalError (Err Nothing msg) = msg

head' :: String -> [a] -> a
head' _ (x : _) = x
head' msg [] = error msg

tail' :: String -> [a] -> [a]
tail' _ (_ : xs) = xs
tail' msg [] = error msg

-- Type errors
expectedBool :: Pos -> Eval a
expectedBool pos = throwErr (Just pos) "Expected boolean"

expectedInt :: Pos -> Eval a
expectedInt pos = throwErr (Just pos) "Expected integer"

fromVoid :: Pos -> Maybe Value -> Eval Value
fromVoid _ (Just x) = return x
fromVoid pos Nothing = throwErr (Just pos) "Expected non-void function"

-- State utilities
-- Some of these are partial due to using head/tail, but if we somehow
-- find ourselves in a state where we attempt to access an empty
-- stack, then we are going to crash either way.

-- Stack and variable environment utilities
getCallStack :: Eval CallStack
getCallStack = _getCallStack <$> get

putCallStack :: CallStack -> Eval ()
putCallStack s = do
  st <- get
  put $ st {_getCallStack = s}

modifyCallStack :: (CallStack -> CallStack) -> Eval ()
modifyCallStack f = putCallStack . f =<< getCallStack

popCallStack :: Eval ()
popCallStack = modifyCallStack $ tail' "popCallStack: empty call stack"

-- Blocks in current function
getBlocks :: Eval BlockStack
getBlocks = head' "getBlocks: empty block stack" <$> getCallStack

putBlock :: BlockStack -> Eval ()
putBlock b = modifyCallStack $ (b :) . tail' "putBlock: empty block stack"

modifyBlocks :: (BlockStack -> BlockStack) -> Eval ()
modifyBlocks f = putBlock . f =<< getBlocks

popBlock :: Eval ()
popBlock = modifyBlocks $ tail' "popBlock: empty block stack"

newBlock :: Eval ()
newBlock = modifyBlocks (mempty :)

-- Get the "current" (head) environment of the stack
getVarEnv :: Eval VarEnv
getVarEnv = head' "getVarEnv: empty block stack" <$> getBlocks

putVarEnv :: VarEnv -> Eval ()
putVarEnv env = modifyBlocks $ (env :) . tail' "putVarEnv: empty block stack"

modifyVarEnv :: (VarEnv -> VarEnv) -> Eval ()
modifyVarEnv f = putVarEnv . f =<< getVarEnv

pushEmptyFunEnv :: Eval ()
pushEmptyFunEnv = modifyCallStack (mempty :)

newFunction :: VarEnv -> Eval ()
newFunction env = modifyCallStack ([env] :)

lookupVar :: Text -> Eval (Maybe Value)
lookupVar s = findFirst <$> getBlocks
  where
    findFirst [] = Nothing
    findFirst (x : xs) = M.lookup s x <|> findFirst xs

-- Is the variable declared in the current function scope?
isDeclared :: Text -> Eval Bool
isDeclared s = any (M.member s) <$> getBlocks

-- Is the variable declared in the current block scope?
isDeclaredHere :: Text -> Eval Bool
isDeclaredHere s = M.member s <$> getVarEnv

-- Function environment utilities
getFunEnv :: Eval FunEnv
getFunEnv = _getFunEnv <$> get

putFunEnv :: FunEnv -> Eval ()
putFunEnv env = do
  st <- get
  put $ st {_getFunEnv = env}

modifyFunEnv :: (FunEnv -> FunEnv) -> Eval ()
modifyFunEnv f = putFunEnv . f =<< getFunEnv

funIsDeclared :: Text -> Eval Bool
funIsDeclared name = M.member name <$> getFunEnv

evalBoolOp :: Expr -> Bop -> Expr -> Eval (Maybe Value)
evalBoolOp e1 op e2 = do
  when (op /= And && op /= Or) $ error $ "evalBoolOp: invalid op: " <> show op
  left <- evalExpr' e1
  x <- case left of
    BoolVal x' -> return x'
    _ -> expectedBool $ exprPos e1
  let shortCircuit = op == And && not x || op == Or && x
  if shortCircuit
    then return $ Just left
    else do
      right <- evalExpr' e2
      case right of
        BoolVal _ -> return $ Just right
        _ -> expectedBool $ exprPos e2

evalArithOp :: Expr -> (Int -> Int -> Int) -> Expr -> Eval (Maybe Value)
evalArithOp e1 op e2 = do
  left <- evalExpr' e1
  right <- evalExpr' e2
  x <- case left of
    IntVal x' -> return x'
    _ -> expectedInt $ exprPos e1
  y <- case right of
    IntVal y' -> return y'
    _ -> expectedInt $ exprPos e2
  return $ Just $ IntVal $ x `op` y

evalCmpOp :: Expr -> (Int -> Int -> Bool) -> Expr -> Eval (Maybe Value)
evalCmpOp e1 op e2 = do
  left <- evalExpr' e1
  right <- evalExpr' e2
  x <- case left of
    IntVal x' -> return x'
    _ -> expectedInt $ exprPos e1
  y <- case right of
    IntVal y' -> return y'
    _ -> expectedInt $ exprPos e2
  return $ Just $ BoolVal $ x `op` y

evalEqOp :: Bool -> Expr -> Expr -> Eval (Maybe Value)
evalEqOp doNegate e1 e2 = do
  left <- evalExpr' e1
  let negator = if doNegate then not else id
  case left of
    IntVal x -> do
      right <- evalExpr' e2
      case right of
        IntVal y -> return $ Just $ BoolVal $ negator $ x == y
        BoolVal _ -> expectedInt $ exprPos e2
    BoolVal x -> do
      right <- evalExpr' e2
      case right of
        IntVal _ -> expectedBool $ exprPos e2
        BoolVal y -> return $ Just $ BoolVal $ negator $ x == y

evalUop :: Uop -> Expr -> Eval (Maybe Value)
evalUop Not e = do
  res <- evalExpr' e
  b <- case res of
    BoolVal b' -> return b'
    _ -> expectedBool $ exprPos e
  return $ Just $ BoolVal $ not b
evalUop Negation e = do
  res <- evalExpr' e
  x <- case res of
    IntVal x' -> return x'
    _ -> expectedInt $ exprPos e
  return $ Just $ IntVal $ - x

evalBinOp :: Expr -> Bop -> Expr -> Eval (Maybe Value)
evalBinOp e1 Add e2 = evalArithOp e1 (+) e2
evalBinOp e1 Sub e2 = evalArithOp e1 (-) e2
evalBinOp e1 Mul e2 = evalArithOp e1 (*) e2
evalBinOp e1 Div e2 = evalArithOp e1 div e2
evalBinOp e1 Lt e2 = evalCmpOp e1 (<) e2
evalBinOp e1 Lte e2 = evalCmpOp e1 (<=) e2
evalBinOp e1 Gt e2 = evalCmpOp e1 (>) e2
evalBinOp e1 Gte e2 = evalCmpOp e1 (>=) e2
evalBinOp e1 Eq e2 = evalEqOp False e1 e2
evalBinOp e1 Neq e2 = evalEqOp True e1 e2
evalBinOp e1 And e2 = evalBoolOp e1 And e2
evalBinOp e1 Or e2 = evalBoolOp e1 Or e2

evalIdentifier :: Text -> Pos -> Eval (Maybe Value)
evalIdentifier name pos = do
  maybeVal <- lookupVar name
  case maybeVal of
    Just val -> return $ Just val
    Nothing ->
      throwErr (Just pos) $
        "No such identifier: " <> name

assignVar :: Pos -> Text -> Value -> Eval ()
assignVar pos name val = do
  errGuardM (isDeclared name) (Just pos) $
    "assignment to undeclared variable: " <> name
  modifyBlocks findAssign
  where
    findAssign [] = error "assignVar: empty block or unguarded declaration check"
    findAssign (x : xs) =
      if M.member name x
        then M.insert name val x : xs
        else x : findAssign xs

evalAsn :: Text -> Pos -> Expr -> Eval (Maybe Value)
evalAsn name pos e = do
  val <- evalExpr' e
  assignVar pos name val
  return $ Just val

typeOf :: Value -> Type
typeOf (IntVal _) = Int
typeOf (BoolVal _) = Bool

checkArgs :: FormalList -> [Value] -> Bool
checkArgs formals vals =
  all (== True) $ zipWith (\(VarDecl _ t _) val -> t == typeOf val) formals vals

createFunEnv :: FormalList -> [Value] -> Eval ()
createFunEnv formals vals = do
  let newEnv =
        M.fromList $
          zipWith (\(VarDecl s _ _) val -> (s, val)) formals vals
  newFunction newEnv

lookupFun :: Text -> Pos -> Eval Function
lookupFun name pos = do
  fun <- M.lookup name <$> getFunEnv
  case fun of
    Just f -> return f
    Nothing -> throwErr (Just pos) $ "No such function: " <> name

evalFuncall :: Text -> Pos -> [Expr] -> Eval (Maybe Value)
evalFuncall name pos exprs = do
  f <- lookupFun name pos
  vals <- mapM evalExpr' exprs
  case f of
    Def t formals block _ -> do
      errGuard
        ( checkArgs formals vals
            && length formals == length vals
        )
        (Just pos)
        "Arg type mismatch"
      createFunEnv formals vals
      res <- evalBlock block
      popCallStack
      case res of
        Val v@(Just x) -> do
          let t' = typeOf x
          errGuard (t == t') (Just pos) $
            "Expected " <> T.pack (show t) <> " result, got " <> T.pack (show t')
          return v
        Val Nothing -> do
          errGuard (t == Void) (Just pos) $
            "Expected " <> T.pack (show t) <> " result, got void"
          return Nothing
        Cont -> return Nothing
    Print -> builtinPrint vals >> return Nothing

evalExpr' :: Expr -> Eval Value
evalExpr' e = evalExpr e >>= fromVoid (exprPos e)

evalExpr :: Expr -> Eval (Maybe Value)
evalExpr (IntExpr x _) = return $ Just $ IntVal x
evalExpr (BoolExpr b _) = return $ Just $ BoolVal b
evalExpr (Identifier name pos _) = evalIdentifier name pos
evalExpr (Bop e1 op e2 _ _) = evalBinOp e1 op e2
evalExpr (Uop op e _) = evalUop op e
evalExpr (Assignment var e pos _) = evalAsn var pos e
evalExpr (Funcall name exprs pos _) = evalFuncall name pos exprs

evalBlock :: [Stmt] -> Eval Ret
evalBlock stmts = do
  newBlock
  res <- evalBlock' stmts
  popBlock
  return res
  where
    evalBlock' [] = return Cont
    evalBlock' (x : xs) = do
      res <- evalStmt x
      case res of
        x'@(Val _) -> return x'
        Cont -> evalBlock' xs

defVal :: Type -> Value
defVal Int = IntVal 0
defVal Bool = BoolVal False
defVal Void = error "no default value for void"

evalStmt :: Stmt -> Eval Ret
evalStmt (Block stmts _) = evalBlock stmts
evalStmt (If expr stmt _) =
  evalExpr' expr
    >>= \case
      BoolVal x -> if x then evalStmt stmt else return Cont
      _ -> expectedBool $ exprPos expr
evalStmt (IfElse expr s1 s2 _) =
  evalExpr' expr
    >>= \case
      BoolVal x -> if x then evalStmt s1 else evalStmt s2
      _ -> expectedBool $ exprPos expr
evalStmt stmt'@(While expr stmt _) =
  evalExpr' expr
    >>= \case
      BoolVal x ->
        if x
          then evalStmt stmt >> evalStmt stmt'
          else return Cont
      _ -> expectedBool $ exprPos expr
evalStmt (Return (Just expr) _ _) = do
  maybeVal <- evalExpr expr
  return $ Val maybeVal
evalStmt (Return Nothing _ _) = return $ Val Nothing
evalStmt (ExprStmt e) = evalExpr e >> return Cont
evalStmt (Variable (VarDecl name t pos)) = do
  errGuardM (not <$> isDeclaredHere name) (Just pos) $
    "multiple declarations of variable " <> name
  modifyVarEnv $ M.insert name $ defVal t
  return Cont

evalDecl :: Decl -> Eval ()
evalDecl f@(Decl _ name formals _ pos) = do
  guardDuplicateParams name formals
  errGuardM (not <$> funIsDeclared name) (Just pos) $
    "Multiple definitions of function " <> name
  modifyFunEnv $ M.insert name $ declToFun f
