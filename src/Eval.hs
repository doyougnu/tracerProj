module Eval where

import Data.Map as M hiding (mapMaybe)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

import Lang

-- | formatting, given a statement, the result of an eval, and state, function
-- formats for pretty output
format :: Show a => Stmt -> a -> VarState -> String
format e res st = " " ++ show e ++ " State: " ++ show st ++
                  "\n => " ++ show res ++ "\n\n"

-- | Wrapper for nicely outputted writer monad logs
noComment :: String
noComment = ""

tellM :: (Show a, MonadWriter String m) => Stmt -> a -> VarState -> m ()
tellM = ((tell .) .) . format

-- | Eval Binary Boolean Operators
evalBBOp :: BinBoolOp -> Bool -> Bool -> Bool
evalBBOp And = (&&)
evalBBOp Or  = (||)

-- | Eval Binary Arithmetic Operators
evalAROp :: (PNum a) => ArBinOp -> a -> a -> a
evalAROp Add      = (+)
evalAROp Subtract = (-)
evalAROp Multiply = (*)
evalAROp Divide   = (./)

-- | Eval Relational Binary Boolean Operators
evalRBOp :: (PNum a, Ord a) => RelBoolOp -> a -> a -> Bool
evalRBOp Less    = (<)
evalRBOp Greater = (>)
evalRBOp Equal   = (==)
evalRBOp NEqual  = (/=)

-- | Helper functions to wrap literals into Stmts
bPack :: Bool -> StateM Stmt
bPack = return . BL . B

aPack :: (PNum a, Integral a) => a -> StateM Stmt
aPack = return . AR . I . toInteger

-- | Denotational Semantics of Boolean Expressions
evalBExpr :: BoolExpr -> StateM Stmt
evalBExpr (B b)             = bPack b
evalBExpr (Not b)           = do
  (BL (B b')) <- evalBExpr b
  bPack $ not b'
evalBExpr (BBinary op b b') = do
  (BL (B l)) <- evalBExpr b
  (BL (B r)) <- evalBExpr b'
  bPack $ evalBBOp op l r
evalBExpr (RBinary op a a') = do
  (AR (I i)) <-  evalAExpr a
  (AR (I i')) <- evalAExpr a'
  bPack $ evalRBOp op i i'
evalBExpr BNoOp = return NoOp

-- | Denotational Semantics of Arithmetic Expressions
evalAExpr :: ArExpr -> StateM Stmt
evalAExpr (V var)          = do
  st <- get
  case M.lookup var st of
    Just res -> return res
    Nothing  -> tell ("Failed to Lookup var: " ++ var) >> mzero
evalAExpr (I i)            = aPack i
evalAExpr (Neg i)          = do
  (AR (I i')) <- evalAExpr i
  aPack $ negate i'
evalAExpr (ABinary op l r) = do
  (AR (I l')) <- evalAExpr l
  (AR (I r')) <- evalAExpr r
  aPack $ evalAROp op l' r'
evalAExpr ArNoOp = return NoOp

-- | Denotational Semantics of Statements
eval :: Stmt -> StateM Stmt
eval (BL b)   = evalBExpr b
eval (AR a)   = evalAExpr a
eval a@(ST _) = return a
eval s@(Let var stmt) = do
  st <- get
  e <- eval stmt
  let st' = M.insert var e st
  put st'
  tellM s st' st
  return NoOp

eval (If b s s') = do
  (BL (B b')) <- evalBExpr b
  if b'
    then eval s
    else eval s'

eval e@(While cond body) = do
  st <- get
  tellM e noComment st
  (BL (B cond')) <- evalBExpr cond
  if cond'
    then do
    eval body
    eval e
    else return NoOp

eval (Seq xs) = mapM eval xs >>= return . last
eval e = return e

-- | Running the monad stack
runLang :: Stmt -> VarState -> ((Maybe Stmt, Log), VarState)
runLang = runState . runWriterT  . runMaybeT . eval

-- | Get the Trace for running the whole program
traceAll :: Stmt -> VarState -> Log
traceAll e s = snd . fst $ runLang e s

-- | Given a trace, output the trace, prettily
printTrace :: Log -> IO ()
printTrace = mapM_ putStrLn . lines

-- | Get the final result, and final state
runProg :: Stmt -> VarState -> (Maybe Stmt, VarState)
runProg e s = (a, c)
  where ((a, _), c) = runLang e s

-- | Get final result of program
evalProg :: Stmt -> VarState -> Maybe Stmt
evalProg = (fst .) . runProg

-- | Get final state of program
execProg :: Stmt -> VarState -> VarState
execProg = (snd .) . runProg
