module Lang where

import Data.Map as M
import Control.Monad.State

type Var = String

-- Vocabulary
data Lit = LBool Bool
         | LInt Int
         deriving (Show)

data Expr = L Lit
          | V Var
          | Add Expr Expr
          | Not Expr
          | Equal Expr Expr
          | Let Var Expr
          | Seq Expr Expr
          | While Expr Expr
          | NoOp
          deriving (Show)

type GState a = StateT (M.Map Var a) (Maybe) a

-- Testing
emptyState :: M.Map var Expr
emptyState = M.empty

emptyStateM :: GState Expr
emptyStateM = return NoOp

testState :: GState Expr
testState = do
  put $ M.fromList [("x", L (LInt 0)), ("y", L (LInt 10))] 
  return NoOp

x = While (Not (Equal (V "x") (V "y"))) (Let "x" (Add (V "x") (L $ LInt 1)))
testx = runStateT (eval x) $ M.fromList [("x", L (LInt 0)), ("y", L (LInt 10))]

-- Denotational Semantics
eval :: Expr -> GState Expr
eval e@(L _) = return e
eval (V var) = do
  st <- get
  val <- lift . M.lookup var $ st
  return val
eval (Not e) = do
  (L (LBool b)) <- eval e
  return . L. LBool $ not b
eval (Add l r) = do
  (L (LInt l')) <- eval l
  (L (LInt r')) <- eval r
  return . L . LInt $ (l' + r')
eval (Let var exp) = do
  st <- get
  e <- eval exp
  let newst =  M.insert var e st
  put newst
  return NoOp
eval (Seq one two) = do
  eval one
  eval two
eval (Equal l r) = do
  l' <- eval l
  r' <- eval r
  -- This is ugly and basically non monadic
  case l' of
    (L (LInt l'')) -> case r' of
      (L (LInt r'')) -> return . L . LBool $ (l'' == r'')
      _              -> return NoOp
    (L (LBool l''))-> case r' of
      (L (LBool r''))-> return . L . LBool $ (l'' == r'')
      _              -> return NoOp

eval e@(While cond body) = do
  (L (LBool cond')) <- eval cond
  if cond'
    then
    do
    eval body
    eval e
    else return NoOp
eval e = return e
