module Main where

import Parser
import Lang
import Eval
import Trace
import Text.Megaparsec (parse, parseErrorPretty)
import Data.Text (pack)

main :: IO ()
main = return ()

inputProg :: String -> (Stmt -> IO ()) -> IO ()
inputProg file f = do
  program <- readFile $ "Programs/" ++ file ++ ".txt"
  let ast = parse langParser "" $ pack program
  case ast of
    Left err   -> putStr (parseErrorPretty err)
    Right ast' -> f ast'

traceTotalProg :: String -> IO ()
traceTotalProg file = inputProg file (printTrace . flip traceAll emptyState)

sliceAndTrace :: String -> Var -> IO ()
sliceAndTrace file var = inputProg file (printTrace .
                                         flip traceAll emptyState .
                                         holify var)

printRunProg :: String -> IO ()
printRunProg file = inputProg file (print . flip runProg emptyState)

printEvalProg :: String -> IO ()
printEvalProg file = inputProg file (print . flip evalProg emptyState)

printExecProg :: String -> IO ()
printExecProg file = inputProg file (print . flip execProg emptyState)
