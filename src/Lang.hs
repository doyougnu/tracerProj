module Lang where

import Data.Map as M
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

type Var = String

-- Vocabulary
data Lit = LBool Bool
         | LInt Int

data Expr = L Lit
          | V Var
          | Add Expr Expr
          | Not Expr
          | Equal Expr Expr
          | Let Var Expr
          | Seq Expr Expr
          | While Expr Expr
          | NoOp

-- Types
type StateV = M.Map Var Expr
type Log = String
type StateM = WriterT Log (StateT (StateV) (Maybe)) Expr

-- Testing
emptyState :: StateV
emptyState = M.empty

testState :: StateV
testState = M.fromList [("x", L (LInt 0)), ("y", L (LInt 10))]

x = While (Not (Equal (V "x") (V "y"))) (Let "x" (Add (V "x") (L $ LInt 1)))

-- Denotational Semantics
eval :: Expr -> StateM
eval e@(L _) = return e
eval (V var) = do
  st <- get
  val <- lift . lift . M.lookup var $ st
  return val
eval (Not e) = do
  (L (LBool b)) <- eval e
  return . L. LBool $ not b
eval e@(Add l r) = do
  (L (LInt l')) <- eval l
  (L (LInt r')) <- eval r
  let res = l' + r'
  tell $ show e ++ " : " ++ show res ++ "\n"
  return . L . LInt $ res
eval (Let var exp) = do
  st <- get
  e <- eval exp
  put $ M.insert var e st
  return NoOp
eval (Seq one two) = do
  eval one
  eval two
eval (Equal l r) = ints <|> bools
  where ints = do
          (L (LInt l')) <- eval l
          (L (LInt r')) <- eval r
          return . L. LBool $ l' == r'
        bools = do
          (L (LBool l')) <- eval l
          (L (LBool r')) <- eval r
          return . L. LBool $ l' == r'
eval e@(While cond body) = do
  (L (LBool cond')) <- eval cond
  if cond'
    then do
    eval body
    eval e
    else return NoOp
eval e = return e

-- Running the monad stack
runLang :: Expr -> StateV -> Maybe ((Expr, Log), StateV)
runLang e s = runStateT (runWriterT $ eval e) s

-- Pretty Printing instance
instance Show Lit where
  show (LBool b) = show b
  show (LInt i)  = show i

instance Show Expr where
  show (L l) = show l
  show (V v) = show v
  show (Add e e') = show e ++ " + " ++ show e'
  show (Not e) = "~" ++ show e
  show (Equal e e') = show e ++ " == " ++ show e'
  show (Let v e) = show v ++ " = " ++ show e
  show (Seq e e') = show e ++ "; " ++ show e'
  show (While e e') = "while (" ++ show e ++ ")" ++ "{\n" ++ show e' ++ "}\n"
  show NoOp = ""
