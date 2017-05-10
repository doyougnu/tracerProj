module Lang where

import Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

type Var = String

-- Vocabulary
data Lit = LBool Bool
         | LInt Int

data Stmt = L Lit
          | V Var
          | Add Stmt Stmt
          | Not Stmt
          | Equal Stmt Stmt
          | Let Var Stmt
          | Seq [Stmt]
          | While Stmt Stmt
          | NoOp

-- Types
type StateV = M.Map Var Stmt
type Log = String
type StateM = WriterT Log (StateT (StateV) (Maybe)) Stmt

-- Testing
emptyState :: StateV
emptyState = M.empty

testState :: StateV
testState = M.fromList [("x", L (LInt 0)), ("y", L (LInt 10))]

x :: Stmt
x = While (Not (Equal (V "x") (V "y"))) (Let "x" (Add (V "x") (L $ LInt 1)))

y :: Stmt
y = Seq [Equal (V "x") (L (LInt 10)), x, Equal (V "x") (L (LInt 10))]

-- Denotational Semantics
eval :: Stmt -> StateM
eval e@(L _) = return e
eval e@(V var) = do
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
  st <- get
  tellM e res st
  return . L . LInt $ res
eval (Let var expr) = do
  st <- get
  e <- eval expr
  put $ M.insert var e st
  return NoOp
eval (Seq xs) = mapM_ eval xs >> return NoOp
eval e@(Equal l r) = ints <|> bools
  where ints = do
          (L (LInt l')) <- eval l
          (L (LInt r')) <- eval r
          let res = L . LBool $ l' == r'
          st <- get
          tellM e res st
          return res
        bools = do
          (L (LBool l')) <- eval l
          (L (LBool r')) <- eval r
          let res = L . LBool $ l' == r'
          st <- get
          tellM e res st
          return res

eval e@(While cond body) = do
  (L (LBool cond')) <- eval cond
  st <- get
  tellM e "" st
  if cond'
    then do
    eval body
    eval e
    else return NoOp
eval e = return e

-- Running the monad stack
runLang :: Stmt -> StateV -> Maybe ((Stmt, Log), StateV)
runLang e = runStateT (runWriterT $ eval e)

-- Pretty Printing
instance Show Lit where
  show (LBool b) = show b
  show (LInt i)  = show i

instance Show Stmt where
  show (L l) = show l
  show (V v) = show v
  show (Add e e') = show e ++ " + " ++ show e'
  show (Not e) = "!" ++ "(" ++ show e ++ ")"
  show (Equal e e') = show e ++ " == " ++ show e'
  show (Let v e) = show v ++ " = " ++ show e
  show (Seq xs) = concatMap (\x -> show x ++ ";\n") xs
  show (While e e') = "while (" ++ show e ++ ")"
                      ++ "{\n" ++ "  " ++ show e' ++ "\n}\n"
  show NoOp = "\n"

format :: Show a => Stmt -> a -> StateV -> String
format e res st = "  " ++ show e ++ " => " ++ show res ++ " State: " ++
                  show st ++ "\n"

traceAll :: Stmt -> StateV -> Log
traceAll e s = snd . fst . fromJust $ runLang e s

printTrace :: Log -> IO ()
printTrace = mapM_ putStrLn . lines

tellM :: (Show a, MonadWriter String m) => Stmt -> a -> StateV -> m ()
tellM = ((tell .) .) . format
