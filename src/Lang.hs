module Lang where

import Data.Map
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

type Var = String

-- *  Booleans
-- | Binary Boolean Ops
data BinBoolOp = And | Or
               deriving (Eq, Ord)
               -- deriving (Eq, Show)

-- | Relational Boolean Ops
data RelBoolOp = Less
               | Greater
               | Equal
               | NEqual
               deriving (Eq, Ord)
               -- deriving (Eq, Show)

-- | Boolean Expressions
data BoolExpr  = B Bool
              | Not BoolExpr
              | BBinary BinBoolOp BoolExpr BoolExpr
              | RBinary RelBoolOp ArExpr ArExpr
              | BNoOp
              deriving (Eq, Ord)
              -- deriving (Eq, Show)

-- * Arithmetic Operators
-- | Binary Arithmatic operators
data ArBinOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Eq, Ord)
             -- deriving (Eq, Show)

-- | Arithmetic Expressions
data ArExpr  = V Var
            | I Integer
            | Neg ArExpr
            | ABinary ArBinOp ArExpr ArExpr
            | ArNoOp
            deriving (Eq, Ord)
            -- deriving (Eq, Show)

-- * Statements
data Stmt  = BL BoolExpr
          | AR ArExpr
          | ST String
          | Let String Stmt
          | If BoolExpr Stmt  Stmt
          | While BoolExpr Stmt
          | Seq [Stmt ]
          | NoOp
          deriving (Eq, Ord)
          -- deriving (Eq, Show)

-- | Types
type VarState  = Map Var Stmt
type Log = String

type LangMonad s w a = MaybeT (WriterT w (State s)) a

type StateM a = LangMonad VarState Log a

-- Testing
emptyState :: VarState
emptyState = empty

testState :: VarState
testState = fromList [("x", AR (I 0)), ("y", AR (I 10))]

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

-- -- | Pretty Printing
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
  show (ST s)            = s
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
