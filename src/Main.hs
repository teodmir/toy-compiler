{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codegen
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Evaluator
import Parser (Program, parseProgram)
import Prettyprinter
import SharedUtils (Err)
import System.Environment
import TypeChecker

-- TODO new codegen
-- format strings
-- check pretty printer
-- check evaluator
-- cleanup

evalText :: String -> IO ()
evalText s =
  runEvaluator (parseProgram s) >>= \case
    Right () -> return ()
    Left e -> print $ formatEvalError e

evalFile :: String -> IO ()
evalFile fname = readFile fname >>= evalText

typeCheckText :: (Err -> Text) -> (Program -> IO ()) -> String -> IO ()
typeCheckText formatter action s =
  case runTypechecker $ parseProgram s of
    Right tree -> action tree
    Left e -> print $ formatter e

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-p", filename] -> print . runPP . parseProgram =<< readFile filename
    ["-e", filename] -> evalFile filename
    ["-d", filename] ->
      readFile filename
        >>= typeCheckText
          formatTypeErrorNicer
          (TIO.writeFile "DEBUG" . compileWithLabels)
    [filename] ->
      readFile filename
        >>= typeCheckText
          formatTypeErrorNicer
          (TIO.writeFile "a.out" . compileWithLines)
    _ -> error "Unknown args format"
