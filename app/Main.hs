module Main where

import Parser
import Lang
import Eval
import Trace
import Text.Megaparsec (parse, parseErrorPretty)
import Data.Text (pack)

main :: IO ()
main = return ()

-- | Helper function that takes a file name, and a function to apply to an AST
-- Once one is parsed
inputProg :: String -> (Stmt -> IO ()) -> IO ()
inputProg file f = do
  program <- readFile $ "Programs/" ++ file ++ ".txt"
  let ast = parse langParser "" $ pack program
  print ast
  case ast of
    Left err   -> putStr (parseErrorPretty err)
    Right ast' -> f ast'

-- | Run the program and trace all computations
traceTotalProg :: String -> IO ()
traceTotalProg file = inputProg file (printTrace . flip traceAll emptyState)

-- | Forward slice the program based on a Variable and trace all computations
sliceAndTrace :: String -> Var -> IO ()
sliceAndTrace file var = inputProg file (printTrace .
                                         flip traceAll emptyState .
                                         holify var)

-- | Run the program and print the final value and final state
printRunProg :: String -> IO ()
printRunProg file = inputProg file (print . flip runProg emptyState)

-- | Run the program and print the final value
printEvalProg :: String -> IO ()
printEvalProg file = inputProg file (print . flip evalProg emptyState)

-- | Run the program and print the final state
printExecProg :: String -> IO ()
printExecProg file = inputProg file (print . flip execProg emptyState)

-- Implement line numbers so we do not have to print out the entire program
-- Allow a list of variable inputs
-- How did FP support the program or how did it hinder it?
