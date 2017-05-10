module Lang where

import Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

type Var = String

-- *  Booleans
-- | Binary Boolean Ops
data BinBoolOp = And | Or deriving (Show)

-- | Relational Boolean Ops
data RelBoolOp = Less
               | Greater
               | Equal
               deriving (Show)

-- | Boolean Expressions
data BoolExpr = B Bool
              | Not BoolExpr
              | BBinary BinBoolOp BoolExpr BoolExpr
              | RBinary RelBoolOp ArExpr ArExpr
              deriving (Show)

-- * Arithmetic Operators
-- | Binary Arithmatic operators
data ArBinOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Show)

-- | Arithmetic Expressions
data ArExpr = I Integer
            | Neg ArExpr
            | ABinary ArBinOp ArExpr ArExpr
            deriving (Show)

-- * Statements
data Stmt = V Var
          | BL BoolExpr
          | AR ArExpr
          | Let String Stmt
          | If BoolExpr Stmt Stmt
          | While BoolExpr Stmt
          | Seq [Stmt]
          | NoOp
          deriving (Show)


-- Types
type StateV = M.Map Var Stmt
type Log = String
type StateM a = WriterT Log (StateT (StateV) (Maybe)) a

-- Testing
emptyState :: StateV
emptyState = M.empty

-- testState :: StateV
-- testState = M.fromList [("x", L (LInt 0)), ("y", L (LInt 10))]

-- x :: Stmt
-- x = While (Not (Equal (V "x") (V "y"))) (Let "x" (Add (V "x") (L $ LInt 1)))

-- y :: Stmt
-- y = Seq [Equal (V "x") (L (LInt 10)), x, Equal (V "x") (L (LInt 10))]

-- Denotational Semantics
-- eval :: Stmt -> StateM
-- eval e@(L _) = return e
-- eval e@(V var) = do
--   st <- get
--   val <- lift . lift . M.lookup var $ st
--   return val
-- eval (Not e) = do
--   (L (LBool b)) <- eval e
--   return . L. LBool $ not b
-- eval e@(Add l r) = do
--   (L (LInt l')) <- eval l
--   (L (LInt r')) <- eval r
--   let res = l' + r'
--   st <- get
--   tellM e res st
--   return . L . LInt $ res
-- eval (Let var expr) = do
--   st <- get
--   e <- eval expr
--   put $ M.insert var e st
--   return NoOp
-- eval (Seq xs) = mapM_ eval xs >> return NoOp
-- eval e@(Equal l r) = ints <|> bools
--   where ints = do
--           (L (LInt l')) <- eval l
--           (L (LInt r')) <- eval r
--           let res = L . LBool $ l' == r'
--           st <- get
--           tellM e res st
--           return res
--         bools = do
--           (L (LBool l')) <- eval l
--           (L (LBool r')) <- eval r
--           let res = L . LBool $ l' == r'
--           st <- get
--           tellM e res st
--           return res

-- eval e@(While cond body) = do
--   (L (LBool cond')) <- eval cond
--   st <- get
--   tellM e "" st
--   if cond'
--     then do
--     eval body
--     eval e
--     else return NoOp
-- eval e = return e

evalBBOp :: BinBoolOp -> Bool -> Bool -> Bool
evalBBOp And = (&&)
evalBBOp Or  = (||)

evalAROp :: (Num a, Fractional a) => ArBinOp -> a -> a -> a
evalAROp Add      = (+)
evalAROp Subtract = (-)
evalAROp Multiply = (*)
evalAROp Divide   = (/)

evalRBOp :: (Ord a) => RelBoolOp -> a -> a -> Bool
evalRBOp (Less)    = (<)
evalRBOp (Greater) = (>)
evalRBOp (Equal)   = (==)

evalBExpr :: BoolExpr -> Bool
evalBExpr (B b)             = b
evalBExpr (Not b)           = not . evalBExpr $ b
evalBExpr (BBinary op b b') = evalBBOp op (evalBExpr b) (evalBExpr b')
evalBExpr (RBinary op a a') = evalRBOp op (evalAExpr a) (evalAExpr a')

evalAExpr :: (Num a, Fractional a) => ArExpr -> a
evalAExpr (I i)            = fromInteger i
evalAExpr (Neg i)          = negate $ evalAExpr i
evalAExpr (ABinary op l r) = evalAROp op (evalAExpr l) (evalAExpr r)


bPack :: Bool -> StateM Stmt
bPack = return . BL . B

aPack :: (Num a, Integral a) => a -> StateM Stmt
aPack = return . AR . I . toInteger

eval :: Stmt -> StateM Stmt
eval e@(V var) = do
  st <- get
  val <- lift . lift . M.lookup var $ st
  return val
eval (BL b) = bPack $ evalBExpr b
eval (AR a) = aPack $ (evalAExpr a ::)
eval (Let var stmt) = do
  st <- get
  e <- eval stmt
  put $ M.insert var e st
  return NoOp
eval (If b s s') = if evalBExpr b
                   then eval s
                   else eval s'
eval e@(While cond body) = do
  st <- get
  tellM e "" st
  if evalBExpr cond
    then do
    eval body
    eval e
    else return NoOp
eval (Seq xs) = mapM_ eval xs >> return NoOp
eval e = return e

-- Running the monad stack
runLang :: Stmt -> StateV -> Maybe ((Stmt, Log), StateV)
runLang e = runStateT (runWriterT $ eval e)

-- Pretty Printing
-- instance Show Lit where
--   show (LBool b) = show b
--   show (LInt i)  = show i

-- instance Show Stmt where
--   show (L l) = show l
--   show (V v) = show v
--   show (Add e e ') = show e ++ " + " ++ show e'
--   show (Not e) = "!" ++ "(" ++ show e ++ ")"
--   show (Equal e e') = show e ++ " == " ++ show e'
--   show (Let v e) = show v ++ " = " ++ show e
--   show (Seq xs) = concatMap (\x -> show x ++ ";\n") xs
--   show (While e e') = "while (" ++ show e ++ ")"
--                       ++ "{\n" ++ "  " ++ show e' ++ "\n}\n"
--   show NoOp = "\n"

format :: Show a => Stmt -> a -> StateV -> String
format e res st = "  " ++ show e ++ " => " ++ show res ++ " State: " ++
                  show st ++ "\n"

traceAll :: Stmt -> StateV -> Log
traceAll e s = snd . fst . fromJust $ runLang e s

printTrace :: Log -> IO ()
printTrace = mapM_ putStrLn . lines

tellM :: (Show a, MonadWriter String m) => Stmt -> a -> StateV -> m ()
tellM = ((tell .) .) . format
