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

-- | Relational Boolean Ops
data RelBoolOp = Less
               | Greater
               | Equal
               | NEqual
               deriving (Eq, Ord)

-- | Boolean Expressions
data BoolExpr = B Bool
              | Not BoolExpr
              | BBinary BinBoolOp BoolExpr BoolExpr
              | RBinary RelBoolOp ArExpr ArExpr
              | BNoOp
              deriving (Eq, Ord)

-- * Arithmetic Operators
-- | Binary Arithmatic operators
data ArBinOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Eq, Ord)

-- | Arithmetic Expressions
data ArExpr  = V Var
            | I Integer
            | Neg ArExpr
            | ABinary ArBinOp ArExpr ArExpr
            | ArNoOp
            deriving (Eq, Ord)

-- * Statements
data Stmt = BL BoolExpr
          | AR ArExpr
          | ST String
          | Let String Stmt
          | If BoolExpr Stmt  Stmt
          | While BoolExpr Stmt
          | Seq [Stmt]
          | NoOp
          deriving (Eq, Ord)

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

badwhileTest :: Stmt
badwhileTest = While (Not (RBinary Equal (V "x") (V "y")))
  (Let "x" (AR (ABinary Add (V "x") (I 1))))

whileTest :: Stmt
whileTest = Seq [ Let "x" (AR $ I 0)
                , Let "y" (AR $ I 100)
                , Let "x" (AR $ I 5)
                , While (Not (RBinary Equal (V "x") (V "y")))
                  (Let "x" (AR (ABinary Add (V "x") (I 1))))
                ]

ifTest :: Stmt
ifTest = Seq [ Let "aa" (AR $ I 0)
             , Let "bb" (AR $ I 100)
             , If (RBinary Less (V "aa") (V "bb"))
               (Seq [ AR (ABinary Subtract (V "bb") (I 10))
                    , Let "aa" (AR . Neg $ V "aa")
                    ])
               NoOp
             ]

ifTest2 :: Stmt
ifTest2 = Seq [ Let "aa" (AR $ I 0)
             , Let "bb" (AR $ I 100)
             , If (RBinary Less (V "aa") (V "bb"))
               (Seq [ AR (ABinary Subtract (V "bb") (I 10))
                    , Let "aa" (AR . Neg $ V "aa")
                    ])
               (Seq [ Let "aa" (AR . I $ 1000)])
             ]
whileTest2 :: Stmt
whileTest2 = Seq [Let "x" (AR (I 0)),Let "z" (AR (I 10)),Let "x" (AR (V "z")),Let "z" (AR (V "x")),While (RBinary NEqual (V "x") (I 5)) (Seq [Let "x" (AR (ABinary Add (V "x") (I 1))),Let "z" (ST "String")])]

seqTest :: Stmt
seqTest = Seq [Let "x" (AR (I 2)),Let "y" (AR (I 3)),Let "x" (AR (V "y")),NoOp]

addTest :: Stmt
addTest = Seq [Let "x" (AR (I 2)), AR (ABinary Add (V "x") (V "x"))]

letTest :: Stmt
letTest = Seq [ Let "x" (AR $ I 2)
              , Let "y" (AR $ V "x")
              , Let "x" (AR $ I 11)
              , Let "z" (AR (ABinary Add (V "x") (V "y")))
              ]
--

-- | Wrapping division into Num to avoid Fractional
class Num n => PNum n where
  (./) :: n -> n -> n

-- | Definition for Integers
instance PNum Integer where
  (./) = div

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
  show (Neg a)           = "~" ++ show a
  show (ABinary op a a') = show a ++ " " ++ show op ++ " " ++ show a'
  show ArNoOp             = ""

instance Show Stmt where
  show (BL b)            = show b
  show (AR a)            = show a
  show (ST s)            = s
  show (Let str stmt)    = "let " ++ str ++ " = " ++ show stmt
  show (If cond t e)     = "If (" ++ show cond ++ ") {\n"  ++ buffer t ++ "} "
                           ++ "else {\n" ++ buffer e ++ "}\n"
  show (While cond stmt) = "while (" ++ show cond ++ ") {\n" ++
                           buffer stmt ++ "}"
  show (Seq xs)          = concatMap (flip (++) "\n". show) xs
  show NoOp              = ""

buffer :: Stmt -> String
buffer stmt = case stmt of
                 Seq xs -> concatMap (\s -> "    " ++ show s ++ "\n") xs
                 x      -> "    " ++ show x
