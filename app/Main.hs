module Main where

import Parser
import Lang
import Text.Megaparsec (parse, parseErrorPretty, parseTest)
import Data.Text (pack)
import System.Directory

main :: IO ()
main = do
  program <- readFile "Programs/WhileStatement.txt"
  let ast = parse langParser "" $ pack program
  case ast of
    Left err   -> putStr (parseErrorPretty err)
    Right ast' -> printTrace $ traceAll ast' emptyState
