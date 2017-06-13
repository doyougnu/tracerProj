module Main where

import Data.Text (pack)
import Data.List (sort)
import System.Directory (getDirectoryContents)
import Text.Megaparsec (parse, parseErrorPretty)

import Parser
import Lang
import Eval

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

-- | Slice a program by a given variable, P denotes program is not in Programs/
sliceP :: String -> Var ->  IO ()
sliceP file var = input file (print . C.genAST var)

-- | Slice a program then Evaluate it, P denotes program is not in Programs/
sliceAndEvalP :: String -> Var -> IO ()
sliceAndEvalP file var = input file (print .
                                      flip evalProg emptyState .
                                      C.genAST var)

-- | Slice a program then Evaluate it and trace the evaluation
sliceTraceEvalP :: String -> Var -> IO ()
sliceTraceEvalP file var = input file (printTrace .
                                        flip traceAll emptyState .
                                        C.genAST var)

-- | Slice a program by a given var, with the assumption that prog is in Programs/
slice :: String -> Var ->  IO ()
slice file var = inputProg file (print . C.genAST var)

-- | Slice a program and evaluate the resultant program
sliceAndEval :: String -> Var -> IO ()
sliceAndEval file var = inputProg file (print .
                                        flip evalProg emptyState .
                                        C.genAST var)

-- | Slice a program, evaluate the resultant program, and then trace the eval.
sliceTraceEval :: String -> Var -> IO ()
sliceTraceEval file var = inputProg file (printTrace .
                                         flip traceAll emptyState .
                                         C.genAST var)

-- | Helper function to remind user of options
help :: IO ()
help = mapM_ putStrLn [ "Available Functions: "
                      , "  1. Programs        -- List all available pre-made programs in the Programs/ directory"
                      , "  2. printProgram    -- Print a given program"
                      , "  3. slice           -- Slice a program on a given variable"
                      , "  4. sliceAndEval    -- Slice a program, then evaluate the resultant program"
                      , "  5. sliceTraceEval  -- Slice a program, and evaluate the result while tracing it"
                      , "  6. sliceP          -- Same as 3, but assumes the program is not in the Programs/ directory"
                      , "  7. sliceAndEvalP   -- Same as 4, but makes the same assumption as 6"
                      , "  8. sliceTraceEvalP -- Same as 5, makes same assumptions as 6, 7"
                      , "  9. printRunProg    -- Run the program, print the final value and final state"
                      , " 10. printEvalProg   -- Run the program, print the final value"
                      , " 11. printExecProg   -- Run the program, print the final state"
                      , " 12. printRunProgP   -- see P-variants above"
                      , " 13. printEvalProgP  -- see P-variants above"
                      , " 14. printExecProgP  -- see P-variants above"
                      ]

-- | list all the programs available to run
programs :: IO ()
programs = getDirectoryContents "Programs/" >>= mapM_ putStrLn . drop 2 . sort

-- | Given a filename, print the program in that file
printProgram :: String -> IO ()
printProgram file = inputProg file print

-- | Run the program and print the final value and final state
printRunProg :: String -> IO ()
printRunProg file = inputProg file (print . flip runProg emptyState)

-- | Run the program and print the final value
printEvalProg :: String -> IO ()
printEvalProg file = inputProg file (print . flip evalProg emptyState)

-- | Run the program and print the final state
printExecProg :: String -> IO ()
printExecProg file = inputProg file (print . flip execProg emptyState)

-- | Run the program and print the final value and final state
printRunProgP :: String -> IO ()
printRunProgP file = input file (print . flip runProg emptyState)

-- | Run the program and print the final value
printEvalProgP :: String -> IO ()
printEvalProgP file = input file (print . flip evalProg emptyState)

-- | Run the program and print the final state
printExecProgP :: String -> IO ()
printExecProgP file = input file (print . flip execProg emptyState)
