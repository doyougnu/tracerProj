module Main where

import Data.Text (pack)
import Data.List (sort)
import System.Directory (getDirectoryContents)
import Text.Megaparsec (parse, parseErrorPretty)

import Parser
import Lang
import Eval
import Trace
import qualified CFG as C

main :: IO ()
main = return ()

-- | Read a program in as a string, parse it and apply function to AST 
input :: String -> (Stmt -> IO ()) -> IO ()
input file f = do
  program <- readFile file
  let ast = parse langParser "" $ pack program
  case ast of
    Left err   -> putStr (parseErrorPretty err)
    Right ast' -> f ast'

-- | Helper function that takes a file name, and a function to apply to an AST
-- Once one is parsed, this is restricted to running programs from Programs/
inputProg :: String -> (Stmt -> IO ()) -> IO ()
inputProg file = input  ("Programs/" ++ file ++ ".txt")

-- | Run the program and trace all computations
traceTotalProg :: String -> IO ()
traceTotalProg file = inputProg file (printTrace . flip traceAll emptyState)

-- | Forward slice the program based on a Variable and trace all computations
oldSliceAndTrace :: String -> Var -> IO ()
oldSliceAndTrace file var = inputProg file (printTrace .
                                         flip traceAll emptyState .
                                         holify var)

oldSlice :: String -> Var -> IO ()
oldSlice file var = inputProg file (print . holify var)

printProgram :: String -> IO ()
printProgram file = inputProg file print
  
slice :: String -> Var ->  IO ()
slice file var = inputProg file (print . C.genAST var)

sliceAndEval :: String -> Var -> IO ()
sliceAndEval file var = inputProg file (print .
                                        flip evalProg emptyState .
                                        C.genAST var)

sliceTraceEval :: String -> Var -> IO ()
sliceTraceEval file var = inputProg file (printTrace .
                                         flip traceAll emptyState .
                                         C.genAST var)

-- | list all the programs available to run
programs :: IO ()
programs = getDirectoryContents "Programs/" >>= mapM_ putStrLn . drop 2 . sort

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
