module Lang where

import Data.Map as M hiding (mapMaybe)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

type Var = String

-- *  Booleans
-- | Binary Boolean Ops
data BinBoolOp = And | Or
               deriving (Eq)

-- | Relational Boolean Ops
data RelBoolOp = Less
               | Greater
               | Equal
               | NEqual
               deriving (Eq)

-- | Boolean Expressions
data BoolExpr = B Bool
              | Not BoolExpr
              | BBinary BinBoolOp BoolExpr BoolExpr
              | RBinary RelBoolOp ArExpr ArExpr
              | BNoOp
              deriving (Eq)

-- * Arithmetic Operators
-- | Binary Arithmatic operators
data ArBinOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Eq)

-- | Arithmetic Expressions
data ArExpr = V Var
            | I Integer
            | Neg ArExpr
            | ABinary ArBinOp ArExpr ArExpr
            | ArNoOp
            deriving (Eq)

-- * Statements
data Stmt = BL BoolExpr
          | AR ArExpr
          | Let String Stmt
          | If BoolExpr Stmt Stmt
          | While BoolExpr Stmt
          | Seq [Stmt]
          | NoOp
          deriving (Eq)

-- | Types
type VarState = M.Map Var Stmt
type Log = String

type LangMonad s w a = MaybeT (WriterT w (State s)) a

type StateM a = LangMonad VarState Log a

-- Testing
emptyState :: VarState
emptyState = M.empty

testState :: VarState
testState = M.fromList [("x", AR (I 0)), ("y", AR (I 10))]

whileTest :: Stmt
whileTest = While (Not (RBinary Equal (V "x") (V "y")))
  (Let "x" (AR (ABinary Add (V "x") (I 1))))

seqTest :: Stmt
seqTest = Seq [Let "x" (AR (I 2)),Let "y" (AR (I 3)),Let "x" (AR (V "y")),NoOp]

addTest :: Stmt
addTest = Seq [Let "x" (AR (I 2)), AR (ABinary Add (V "x") (V "x"))]
--

-- | Wrapping division into Num to avoid Fractional
class Num n => PNum n where
  (./) :: n -> n -> n

-- | Definition for Integers
instance PNum Integer where
  (./) = div

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
  -- fromMaybe (return NoOp) $ res
  -- maybe (tell ("Failed to Lookup var:" ++ var) >> mzero) (return . fromJust) (M.lookup var st)
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
eval (BL b) = evalBExpr b
eval (AR a) = evalAExpr a
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

eval (Seq xs) = mapM_ eval xs >> return NoOp
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

-- | Pretty Printing
instance Show BinBoolOp where
  show And = "&&"
  show Or  = "||"

instance Show RelBoolOp where
  show Less    = "<"
  show Greater = ">"
  show Equal   = "=="
  show NEqual  = "!="

instance Show BoolExpr where
  show (B b)             = show b
  show (Not b)           = "!" ++ "(" ++ show b ++ ")"
  show (BBinary op b b') = show b ++ " " ++ show op ++ " " ++ show b'
  show (RBinary op a a') = show a ++ " " ++ show op ++ " " ++ show a'
  show BNoOp             = ""

instance Show ArBinOp where
  show Add      = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide   = "/"

instance Show ArExpr where
  show (V var)           = var
  show (I i)             = show i
  show (Neg a)           = "-" ++ show a
  show (ABinary op a a') = show a ++ " " ++ show op ++ " " ++ show a'
  show ArNoOp             = ""

instance Show Stmt where
  show (BL b)            = show b
  show (AR a)            = show a
  show (Let str stmt)    = "let " ++ show str ++ " = " ++ show stmt ++ "\n"
  show (If cond t e)     = "If (" ++ show cond ++ ") {"  ++ show t ++ "} " ++
                           "else {" ++ show e ++ "}\n"
  show (While cond stmt) = "while (" ++ show cond ++ ") {\n" ++
                           ss ++ "}"
    where ss = case stmt of
                 Seq xs -> concatMap (("    " ++) . show) xs
                 x      -> show x
  show (Seq xs)          = concatMap show xs
  show NoOp              = ""

