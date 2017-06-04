module CFG where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Set as S
import Data.List (nub, intersectBy)
import Debug.Trace (trace)

import Lang
import CSP

-- | Two Types of Edges, data dependence, control flow dependence
data Etype = Data
           | Control
           deriving (Show, Eq)

-- | An Edge is either a data edge or Control edge
data Edge t n = Edge t n
  deriving (Show, Eq)

-- TypeClass?
unEdge :: Edge t n -> n
unEdge (Edge _ n) = n

unTEdge :: Edge t n -> t
unTEdge (Edge t _) = t

dataWrap :: n -> Edge Etype n
dataWrap = Edge Data

controlWrap :: n -> Edge Etype n
controlWrap = Edge Control

type Node = Int

-- | An edge in the Control Flow graph, is an edge with an edge type and the node
-- that the edge touches. For a map Map Node Edge the edge is assumed to be from
-- the key node to the node held in the edge type
type DEdge = Edge Etype Node

-- | abstract representation of a Graph constructed by an adjacency Map.
data MGraph n e = MGraph (M.Map n e)
  deriving Show

-- | An integer map where keys are line numbers, values are statements.
type LineMap = I.IntMap Stmt
type LGraph = MGraph Node [DEdge]

-- | Use a reader monad to hold the lineNumber map, state to track graph
type Engine s r a = StateT s (Reader r) a

runEngine i i' = runReader (runStateT i mempty) (toLineMap i')

-- could avoid with newtype + record syntax
unGraph :: MGraph n e -> M.Map n e
unGraph (MGraph xs) = xs

class Graph g n e where
  graph :: [(n, e)] -> g n e
  nodes :: g n e -> [n]
  edges :: g n e -> [(n, e)]
  addNode :: n -> g n e -> g n e
  addNodeWEdge :: n -> e -> g n e -> g n e
  addEdgeWith :: (e -> e -> e) -> n -> e -> g n e -> g n e
  getEdge :: n -> g n e -> e
  alterEdge :: (e -> e) -> n -> g n e -> g n e
  getNodesWith :: (e -> Bool) -> g n e -> [n]
  removeEdgeWith :: (e -> e) -> n -> g n e -> g n e
  removeNode :: n -> g n e -> g n e

class (Graph g n e) => CFG g n e where
  getDataDeps    :: g n e -> [Node]
  getControlDeps :: g n e -> [Node]

instance (Ord n, Eq e, Monoid e) => Graph MGraph n e where
  graph                      = MGraph . M.fromList
  nodes (MGraph xs)          = M.keys xs
  edges (MGraph es)          = M.toList es
  addNode n                  = addEdgeWith mappend n mempty
  addNodeWEdge n e (MGraph es) = MGraph $ M.insert n e es
  addEdgeWith f n e (MGraph es) = MGraph $ M.insertWith f n e es
  getEdge n (MGraph es)     = es M.! n --exception if node not in map
  alterEdge f n (MGraph es)  = MGraph $ M.adjust f n es
  getNodesWith f (MGraph es)     = M.keys $ M.filter f es
  removeEdgeWith f k (MGraph es) = MGraph $ M.adjust f k es
  removeNode k (MGraph es)   = MGraph $ M.delete k es

instance (Ord n) => Monoid (MGraph n [e]) where
  mempty = MGraph M.empty
  mappend (MGraph es) (MGraph es') = MGraph $ M.unionWith (++) es es'


getKeyIM :: Eq a => a -> I.IntMap a -> [Int]
getKeyIM e = I.keys . I.filter (==e)

getKey :: Eq e => e -> M.Map a e -> [a]
getKey e = M.keys . M.filter (==e)

-- Look into Scrap Your Boilerplate for this
-- instance Foldable (DGraph n) where
--   foldr f acc xs = M.foldr f acc (unGraph xs)

-- | Given a variable and a statement, return bool if that variable is ever
-- needed inside the statement
mentions :: Var -> Stmt -> Bool
mentions str (BL (RBinary _ (V s1) (V s2))) = s1 == str || s2 == str
mentions str (AR (V s))                     = str == s
mentions str (AR (ABinary _ a1 a2))         = mentions str (AR a1)
                                              || mentions str (AR a2)

mentions str (Let _ s)                      = mentions str s
mentions str (If c t e)                     = mentions str (BL c)
                                              || mentions str t
                                              || mentions str e

mentions str (While c e)                    = mentions str (BL c)
                                              || mentions str e

mentions str (Seq xs)                       = any (mentions str) xs
mentions _   _                              = False

-- | Given a integer and a statement, return the line number in sequence
-- MONADIFY WHEN YOU HAVE TIME
tag :: Stmt -> Int -> [(Int, Stmt)]
tag (If b t e) n = (n, If b t e) : ts ++ tag e (succ n')
  where ts = tag t (succ n)
        n' = fst . last $ ts
tag (While b e) n = (n, While b e) : tag e (succ n)
-- can't get this fold to work right
-- tag (Seq xs) n = foldr ((++) . flip tag n) [] xs
tag (Seq  [])    _ = [] 
tag (Seq (x:xs)) n = tag x n ++ tag (Seq xs) (succ n)
tag s n = [(n, s)]

-- | Simple helper function passes the stmt to tag with an incremented n
incAndRecur :: Stmt -> Int -> [(Int, Stmt)]
incAndRecur s n= tag s (succ n)

-- | Given a statement, return all variables that are referenced inside that
-- statement
allVars :: Stmt -> S.Set Var
allVars (BL (RBinary _ (V s1) (V s2))) = S.fromList [s1, s2]
allVars (BL (Not b))                   = allVars (BL b)
allVars (Let _ s)                      = allVars s
allVars (AR (V s))                     = S.singleton s
allVars (AR (Neg s))                   = allVars (AR s)
  -- such repetition, much wow
allVars (BL (BBinary _ b1 b2))       = recurAndGet [BL b1, BL b2]
allVars (AR (ABinary _ a1 a2))       = recurAndGet [AR a1, AR a2]
allVars (If c t e)                   = recurAndGet [BL c, t, e]
allVars (While c e)                  = recurAndGet [BL c, e]
allVars (Seq xs)                     = recurAndGet xs
allVars _                            = mempty

-- | allVars helper function to map allVars and convert to Set
recurAndGet :: [Stmt] -> S.Set Var
recurAndGet = S.unions . fmap allVars

-- | given a variable, and the lineMap of the program, return the key that
-- represents the statement that defined the variable, in the case there is more
-- than one, take the most recent that is
-- definedIn :: Var -> Int -> LineMap -> Int
definedIn :: Var -> ([Int] -> [Int]) -> LineMap -> Int
definedIn var f lm = maximum . f . I.keys $ I.filter letDef lm
  where letDef (Let v _) = v == var
        letDef _         = False

-- | Transform a statement to a lineMap, keys are line numbers, values at stmts
toLineMap :: Stmt -> LineMap
toLineMap = I.fromList . flip tag 0

-- | Given a variable and a statement, is the var defined in the stmt?
isLetDef :: String -> Stmt -> Bool
isLetDef var (Let v _) = var == v
isLetDef _   _         = False

-- | node 1 takes all the edges from node 2, node 2's is removed
stealEdges :: Node -> Node -> LGraph -> LGraph
stealEdges n1 n2 g = g'
  where
    e2s = getEdge n2 g
    g' = alterEdge (nub . (++e2s)) n1 g
    -- g'' = removeNode n2 g'

tester1 :: Stmt -> LGraph
tester1 s = fst $ runEngine (toDCFG (tag s 0)) s

tester2 :: Stmt -> LGraph
tester2 s = fst $ runEngine (toCCFG (tag s 0)) s

tester :: Stmt -> LGraph
tester s = mconcat [tester1 s, tester2 s]

-- | Special check for multiple let statements, given a let statement, check if
-- the variable is already assigned, if so then remove old node, and copy over
-- dependencies
existsCheck :: (Int, Stmt) -> Engine LGraph LineMap LGraph
existsCheck (i, Let _  _) =
  do g <- get
     let posNodes = getNodesWith (elem $ dataWrap i) g
     if null posNodes                      -- if empty, then we have a new node
       then return $ addNode i g
       else do return $ addNode i g -- else we had node, then steal
               return $ foldr (stealEdges i) g posNodes
existsCheck _             = get

-- | Helper function for get Dependencies function, recursively find all vars
-- mentioned in the statement, and return them as a list with no duplicates
getDataDeps' :: Stmt -> [Var]
getDataDeps' (Let _ s)   = S.toList $ allVars s
getDataDeps' (If b t e)  = S.toList . S.unions $ allVars <$> [BL b, t, e]
getDataDeps' (While b e) = S.toList . S.unions $ allVars <$> [BL b, e]
getDataDeps' (Seq ss)    = concatMap getDataDeps' ss
getDataDeps' s           = S.toList $ allVars s

-- | given a statement find all dependencies for the statement as a list of nodes
getDeps :: (Int, Stmt) -> Engine LGraph LineMap [Int]
getDeps (i, s) =
  ask >>= \lm -> return . fmap (flip3 definedIn lm (filter (<i))) $ getDataDeps' s
  where flip3 f x y z = f z y x

-- | Given a statement, find all dependencies, and then add them to the graph
addDataDeps :: (Int, Stmt) -> Engine LGraph LineMap LGraph
addDataDeps a@(i, _) = do
  deps <- getDeps a
  helper i deps
  where
    helper node (d:ds) = do
      g <- get
      let g' = addEdgeWith (++) node [dataWrap d] g
      put g'
      helper node ds
    helper _ [] = get

-- | Given a statement, add data edges to that graph
toDCFG :: [(Int, Stmt)] -> Engine LGraph LineMap LGraph
toDCFG [] = get
toDCFG (a@(i, _):ss) = do
  g <- get
  let newGraph = addNode i g
  put newGraph
  existsCheck a
  addDataDeps a
  toDCFG ss

-- | Helper function for get Dependencies function, recursively find all vars
-- mentioned in the statement, and return them as a list with no duplicates

getContDeps' :: (Eq b) => [(a, b)] -> [(a, b)] -> [a]
getContDeps' ss xs = fst <$> intersectBy (\x y -> snd x == snd y) ss xs
  
getContDeps :: (Int, Stmt) -> [Node]
getContDeps (i, If b t e)  = fst <$> tail (tag (If b t e) i)
-- getContDeps ((i, If _ t e):ss)  = getContDeps' ss $ tag t i ++ tag e i
getContDeps (i, While b e) = fst <$> tail (tag (While b e) i)
getContDeps _         = []

addContDeps :: Node -> [Node] -> Engine LGraph LineMap LGraph
addContDeps _ [] = get
addContDeps node (d:ds) = do
        g <- get
        let g' = addEdgeWith (++) d [controlWrap node] g -- slow
        put g'
        addContDeps node ds

-- | Given a statement, and a graph, add control flow edges
toCCFG :: [(Int, Stmt)] -> Engine LGraph LineMap LGraph
toCCFG (b@(i, _):ss) = do
  g <- get
  let deps = getContDeps b
      newGraph = addNode i g
  put newGraph
  addContDeps i deps
  toCCFG ss
toCCFG _ = get
      
data AST var val = AST (Domain Node DEdge)

-- instance CSP AST Node DEdge where
--   vars = variables
  -- domains g = M.fromList $ edges g
