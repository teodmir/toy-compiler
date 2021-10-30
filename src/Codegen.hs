{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen (compileWithLabels, compileWithLines) where

import Control.Monad.Fail ()
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import qualified Data.Text as T
import Parser
import Renamer

data Inst
  = PushInt Int
  | PushBool Bool
  | NegInst
  | NotInst
  | AddInst
  | SubInst
  | MultInst
  | DivInst
  | EqInt
  | EqBool
  | LtInt
  | LeInt
  | AssInt
  | AssBool
  | DeclInst
  | Lval Int
  | RvalInt Int
  | RvalBool Int
  | Brf Label
  | Bra Label
  | Bsr Label
  | BrfLine Int
  | BraLine Int
  | BsrLine Int
  | L Label
  | Pop Int
  | WriteInt
  | WriteBool
  | Link
  | Unlink
  | Rts
  | EndInst

fmtInst :: Inst -> Text
fmtInst (PushInt x) = "PUSHINT " <> T.pack (show x)
fmtInst (PushBool True) = "PUSHBOOL true"
fmtInst (PushBool False) = "PUSHBOOL false"
fmtInst NegInst = "NEG"
fmtInst NotInst = "NOT"
fmtInst AddInst = "ADD"
fmtInst SubInst = "SUB"
fmtInst MultInst = "MULT"
fmtInst DivInst = "DIV"
fmtInst EqInt = "EQINT"
fmtInst EqBool = "EQBOOL"
fmtInst LtInt = "LTINT"
fmtInst LeInt = "LEINT"
fmtInst AssInt = "ASSINT"
fmtInst AssBool = "ASSBOOL"
fmtInst DeclInst = "DECL 1"
fmtInst (Lval x) = "LVAL " <> T.pack (show x) <> "(FP)"
fmtInst (RvalInt x) = "RVALINT " <> T.pack (show x) <> "(FP)"
fmtInst (RvalBool x) = "RVALBOOL " <> T.pack (show x) <> "(FP)"
fmtInst (Brf l) = "BRF " <> l
fmtInst (Bra l) = "BRA " <> l
fmtInst (Bsr l) = "BSR " <> l
fmtInst (L l) = l <> ":"
fmtInst WriteInt = "WRITEINT"
fmtInst WriteBool = "WRITEBOOL"
fmtInst (Pop x) = "POP " <> T.pack (show x)
fmtInst Link = "LINK"
fmtInst Unlink = "UNLINK"
fmtInst Rts = "RTS"
fmtInst EndInst = "END"
fmtInst (BrfLine x) = "BRF " <> T.pack (show x)
fmtInst (BraLine x) = "BRA " <> T.pack (show x)
fmtInst (BsrLine x) = "BSR " <> T.pack (show x)

type Label = Text

type OffsetMap = M.Map Text Int

type LabelMap = M.Map Label Int

type InstList = Seq Inst

data CompileEnv = CompileEnv
  { _getInsts :: InstList,
    _getOffsets :: OffsetMap,
    _getRetOffset :: Int,
    _getNextOffset :: Int,
    _getLabelMap :: LabelMap,
    _getLabelId :: Int,
    _getEmitCount :: Int -- Corresponds to lines in first pass
  }

initEnv :: CompileEnv
initEnv =
  CompileEnv
    { _getInsts = mempty,
      _getOffsets = mempty,
      _getRetOffset = undefined,
      _getNextOffset = -1,
      _getLabelId = 0,
      _getLabelMap = mempty,
      _getEmitCount = 0
    }

newtype Gen a = Gen {runGen :: State CompileEnv a}
  deriving (Functor, Applicative, Monad, MonadState CompileEnv)

-- Didn't have time to read up properly on this, so it probably
-- doesn't obey the required laws; but it's needed for the genLabels
-- pattern matching to work
instance MonadFail Gen where
  fail _ = undefined

runCompiler :: Program -> (Seq Inst, LabelMap)
runCompiler prgm =
  let st = execState (runGen $ compilePrgm $ runRename prgm) initEnv
   in (_getInsts st, _getLabelMap st)

compileWithLabels :: Program -> Text
compileWithLabels prgm =
  let insts = fst $ runCompiler prgm
      formatted = fmap fmtInst insts
   in T.intercalate "\n" $ toList formatted

compileWithLines :: Program -> Text
compileWithLines prgm =
  let (labels, env) = runCompiler prgm
      nonLabeled = transformLabels env $ toList labels
      indexed = zip ([0 ..] :: [Integer]) nonLabeled
      lines' =
        fmap
          (\(line, inst) -> T.pack (show line) <> " " <> fmtInst inst)
          indexed
   in T.intercalate "\n" lines'

getInsts :: Gen InstList
getInsts = gets _getInsts

putInsts :: InstList -> Gen ()
putInsts st = do
  env <- get
  put env {_getInsts = st}

modifyInsts :: (InstList -> InstList) -> Gen ()
modifyInsts f = putInsts . f =<< getInsts

getNextOffset :: Gen Int
getNextOffset = gets _getNextOffset

putNextOffset :: Int -> Gen ()
putNextOffset x = do
  st <- get
  put st {_getNextOffset = x}

getRetOffset :: Gen Int
getRetOffset = gets _getRetOffset

putRetOffset :: Int -> Gen ()
putRetOffset x = do
  st <- get
  put $ st {_getRetOffset = x}

getOffsets :: Gen OffsetMap
getOffsets = gets _getOffsets

putOffsets :: OffsetMap -> Gen ()
putOffsets offs = do
  st <- get
  put st {_getOffsets = offs}

modifyOffsets :: (OffsetMap -> OffsetMap) -> Gen ()
modifyOffsets f = do
  offs <- getOffsets
  putOffsets $ f offs

addOffset :: Text -> Int -> Gen ()
addOffset name offset = modifyOffsets $ M.insert name offset

-- Typechecker guarantees that fromJust can be used here
lookupOffset :: Text -> Gen Int
lookupOffset s = fromJust . M.lookup s <$> getOffsets

getLabelId :: Gen Int
getLabelId = gets _getLabelId

putLabelId :: Int -> Gen ()
putLabelId x = do
  st <- get
  put st {_getLabelId = x}

incLabelId :: Gen ()
incLabelId = putLabelId . (+ 1) =<< getLabelId

genIdLabel :: Text -> Gen Text
genIdLabel s = do
  x <- getLabelId
  return $ s <> "_" <> T.pack (show x)

getEmitCount :: Gen Int
getEmitCount = gets _getEmitCount

putEmitCount :: Int -> Gen ()
putEmitCount x = do
  st <- get
  put $ st {_getEmitCount = x}

modifyEmitCount :: (Int -> Int) -> Gen ()
modifyEmitCount f = putEmitCount . f =<< getEmitCount

getLabelMap :: Gen LabelMap
getLabelMap = gets _getLabelMap

putLabelMap :: LabelMap -> Gen ()
putLabelMap x = do
  st <- get
  put $ st {_getLabelMap = x}

modifyLabelMap :: (LabelMap -> LabelMap) -> Gen ()
modifyLabelMap f = putLabelMap . f =<< getLabelMap

emit :: Inst -> Gen ()
emit i = modifyInsts (|> i) >> modifyEmitCount (+ 1)

emitLabel :: Label -> Gen ()
emitLabel s = do
  x <- getEmitCount
  emit $ L s
  m <- getLabelMap
  if s `M.member` m
    then error "compilation: duplicate labels"
    else modifyLabelMap $ M.insert s x

genLabels :: [Label] -> Gen [Label]
genLabels xs = do
  res <- mapM genIdLabel xs
  incLabelId
  return res

exprType :: Expr -> Type
exprType (IntExpr _ _) = Int
exprType (BoolExpr _ _) = Bool
exprType (Bop _ Add _ _ _) = Int
exprType (Bop _ Sub _ _ _) = Int
exprType (Bop _ Mul _ _ _) = Int
exprType (Bop _ Div _ _ _) = Int
exprType (Bop _ And _ _ _) = Bool
exprType (Bop _ Or _ _ _) = Bool
exprType (Bop _ Eq _ _ _) = Bool
exprType (Bop _ Neq _ _ _) = Bool
exprType (Bop _ Lt _ _ _) = Bool
exprType (Bop _ Gt _ _ _) = Bool
exprType (Bop _ Lte _ _ _) = Bool
exprType (Bop _ Gte _ _ _) = Bool
exprType (Assignment _ _ _ t) = t
exprType (Identifier _ _ t) = t
exprType (Uop Not _ _) = Bool
exprType (Uop Negation _ _) = Int
exprType (Funcall _ _ _ t) = t

compileBop :: Expr -> Expr -> Inst -> Gen ()
compileBop e1 e2 op = compileExpr e1 >> compileExpr e2 >> emit op

compileExpr :: Expr -> Gen ()
compileExpr (IntExpr x _) = emit $ PushInt x
compileExpr (BoolExpr True _) = emit $ PushBool True
compileExpr (BoolExpr False _) = emit $ PushBool False
compileExpr (Uop Negation e _) = compileExpr e >> emit NegInst
compileExpr (Uop Not e _) = compileExpr e >> emit NotInst
compileExpr (Bop e1 Add e2 _ _) = compileBop e1 e2 AddInst
compileExpr (Bop e1 Sub e2 _ _) = compileBop e1 e2 SubInst
compileExpr (Bop e1 Mul e2 _ _) = compileBop e1 e2 MultInst
compileExpr (Bop e1 Div e2 _ _) = compileBop e1 e2 DivInst
compileExpr (Bop e1 Eq e2 _ Int) = compileBop e1 e2 EqInt
compileExpr (Bop e1 Eq e2 _ Bool) = compileBop e1 e2 EqBool
compileExpr (Bop e1 Neq e2 _ Int) = compileBop e1 e2 EqInt >> emit NotInst
compileExpr (Bop e1 Neq e2 _ Bool) = compileBop e1 e2 EqBool >> emit NotInst
compileExpr (Bop e1 Lt e2 _ _) = compileBop e1 e2 LtInt
compileExpr (Bop e1 Gt e2 _ _) = compileBop e1 e2 LeInt >> emit NotInst
compileExpr (Bop e1 Lte e2 _ _) = compileBop e1 e2 LeInt
compileExpr (Bop e1 Gte e2 _ _) = compileBop e1 e2 LtInt >> emit NotInst
compileExpr (Bop e1 Or e2 _ _) = do
  [rightL, endL] <- genLabels ["RIGHT", "END"]
  compileExpr e1
  emit $ Brf rightL
  emit $ PushBool True
  emit $ Bra endL
  emitLabel rightL
  compileExpr e2
  emitLabel endL
compileExpr (Bop e1 And e2 _ _) = do
  [shortL, endL] <- genLabels ["SHORTCIRCUIT", "END"]
  compileExpr e1
  emit $ Brf shortL
  compileExpr e2
  emit $ Bra endL
  emitLabel shortL
  emit $ PushBool False
  emitLabel endL
compileExpr (Identifier name _ Int) = lookupOffset name >>= emit . RvalInt
compileExpr (Identifier name _ Bool) = lookupOffset name >>= emit . RvalBool
compileExpr (Assignment name e _ t) = do
  off <- lookupOffset name
  emit $ Lval off
  compileExpr e
  case t of
    Int -> emit AssInt >> emit (RvalInt off)
    Bool -> emit AssBool >> emit (RvalBool off)
    Void -> error "compilation: assignment to void"
compileExpr (Funcall "print" args _ _) =
  mapM_ (\e -> compileExpr e >> writeType (exprType e)) args
  where
    writeType Int = emit WriteInt
    writeType Bool = emit WriteBool
    writeType Void = error "compilation: attempted to print void"
compileExpr (Funcall name args _ _) = do
  let nargs = length args
  emit DeclInst
  mapM_ compileExpr $ reverse args
  emit $ Bsr name
  emit $ Pop nargs
compileExpr _ = error "compilation: invalid expression"

blockSize :: [Stmt] -> Int
blockSize =
  foldr
    ( \x acc -> case x of
        Variable _ -> acc + 1
        _ -> acc
    )
    0

compileStmt :: Stmt -> Gen ()
compileStmt (Variable (VarDecl name _ _)) = do
  off <- getNextOffset
  putNextOffset $ off - 1
  addOffset name off
  emit DeclInst
compileStmt (Block stmts _) = do
  mapM_ compileStmt stmts
  nextOff <- getNextOffset
  let size = blockSize stmts
  putNextOffset $ nextOff + size
  when (size /= 0) $
    emit $ Pop size
compileStmt (IfElse expr stmt1 stmt2 _) = do
  [elseL, endL] <- genLabels ["ELSE", "END"]
  compileExpr expr
  emit $ Brf elseL
  compileStmt stmt1
  emit $ Bra endL
  emitLabel elseL
  compileStmt stmt2
  emitLabel endL
compileStmt (If expr stmt _) =
  compileStmt $ IfElse expr stmt (Block [] undefined) undefined
compileStmt (While expr stmt _) = do
  [whileL, endL] <- genLabels ["WHILE", "END"]
  emitLabel whileL
  compileExpr expr
  emit $ Brf endL
  compileStmt stmt
  emit $ Bra whileL
  emitLabel endL
compileStmt (ExprStmt e) = compileExpr e
compileStmt (Return Nothing _ _) = emit Unlink >> emit Rts
compileStmt (Return (Just e) _ t) = do
  emit . Lval =<< getRetOffset
  compileExpr e
  case t of
    Int -> emit AssInt
    Bool -> emit AssBool
    Void -> error "compilation: unexpected void return"
  emit Unlink
  emit Rts

compileDecl :: Decl -> Gen ()
compileDecl (Decl _ name formals body _) = do
  putNextOffset (-1) -- Reset next offset
  emitLabel name
  let idxFormals = zip formals [1 ..]
  mapM_ (\(VarDecl name' _ _, i) -> addOffset name' (i + 1)) idxFormals
  putRetOffset $ length formals + 2
  emit Link
  mapM_ compileStmt body
  emit Unlink
  emit Rts

compilePrgm :: [Decl] -> Gen ()
compilePrgm prgms = do
  emit DeclInst
  emit $ Bsr "main"
  emit EndInst
  mapM_ compileDecl prgms

lookupLabel :: Label -> LabelMap -> Int
lookupLabel label m =
  case M.lookup label m of
    Just x -> x
    Nothing -> error "compilation: programmer error: label not found"

transformLabels :: LabelMap -> [Inst] -> [Inst]
transformLabels m insts =
  let (insts', m') = removeLabels insts m
   in fmap (transformBranches m') insts'

removeLabels :: [Inst] -> LabelMap -> ([Inst], LabelMap)
removeLabels xs m =
  first reverse $ removeLabels' ([], m) xs 0 0

-- Remove labels and update their "position" accordingly
removeLabels' :: ([Inst], LabelMap) -> [Inst] -> Int -> Int -> ([Inst], LabelMap)
removeLabels' (ys, m) (L l : xs) line offset =
  -- update map and offset
  let m' = M.adjust (subtract offset) l m
   in removeLabels' (ys, m') xs line (offset + 1)
removeLabels' (ys, m) (x : xs) line offset =
  -- update line
  removeLabels' (x : ys, m) xs (line + 1) offset
removeLabels' (ys, m) [] _ _ = (ys, m)

transformBranches :: LabelMap -> Inst -> Inst
transformBranches m (Brf l) = BrfLine $ lookupLabel l m
transformBranches m (Bra l) = BraLine $ lookupLabel l m
transformBranches m (Bsr l) = BsrLine $ lookupLabel l m
transformBranches _ inst = inst
