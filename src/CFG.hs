module CFG where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Set as S
import qualified Data.List as L
import Data.List (nub)

import Lang

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
  graph          :: [(n, e)] -> g n e
  nodes          :: g n e -> [n]
  edges          :: g n e -> [(n, e)]
  addNode        :: n -> g n e -> g n e
  addNodeWEdge   :: n -> e -> g n e -> g n e
  addEdgeWith    :: (e -> e -> e) -> n -> e -> g n e -> g n e
  getEdge        :: n -> g n e -> e
  alterEdge      :: (e -> e) -> n -> g n e -> g n e
  getNodesWith   :: (e -> Bool) -> g n e -> [n]
  removeEdgeWith :: (e -> e) -> n -> g n e -> g n e
  removeNode     :: n -> g n e -> g n e
  filterGraph    :: (n -> e -> Bool) -> g n e -> g n e 
  filterEdges    :: (e -> Bool) -> g n e -> g n e
  foldrGraph     :: (n -> e -> a -> a) -> a -> g n e -> a
  foldlGraph     :: (a -> n -> e -> a) -> a -> g n e -> a
  foldEdges      :: (e -> a -> a) -> a -> g n e -> a

instance (Ord n, Eq e, Monoid e) => Graph MGraph n e where
  graph                          = MGraph . M.fromList
  nodes (MGraph xs)              = M.keys xs
  edges (MGraph es)              = M.toList es
  addNode n                      = addEdgeWith mappend n mempty
  addNodeWEdge n e (MGraph es)   = MGraph $ M.insert n e es
  addEdgeWith f n e (MGraph es)  = MGraph $ M.insertWith f n e es
  getEdge n (MGraph es)          = es M.! n --exception if node not in map
  alterEdge f n (MGraph es)      = MGraph $ M.adjust f n es
  getNodesWith f (MGraph es)     = M.keys $ M.filter f es
  removeEdgeWith f k (MGraph es) = MGraph $ M.adjust f k es
  removeNode k (MGraph es)       = MGraph $ M.delete k es
  filterGraph p (MGraph es)      = MGraph $ M.filterWithKey p es
  filterEdges p (MGraph es)      = MGraph $ M.filter p es
  foldrGraph p acc (MGraph es)   = M.foldrWithKey p acc es
  foldlGraph p acc (MGraph es)   = M.foldlWithKey p acc es
  foldEdges p acc (MGraph es)    = M.foldr p acc es

instance (Ord n) => Monoid (MGraph n [e]) where
  mempty = MGraph M.empty
  mappend (MGraph es) (MGraph es') = MGraph $ M.unionWith (++) es es'

-- Look into Scrap Your Boilerplate for this
-- instance Foldable (DGraph n) where
--   foldr f acc xs = M.foldr f acc (unGraph xs)

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

-- | Given a statement, return all variables that are referenced inside that
-- statement
allVars :: Stmt -> S.Set Var
allVars (BL (RBinary _ (V s1) (V s2))) = S.fromList [s1, s2]
allVars (BL (Not b))                   = allVars (BL b)
allVars (Let _ s)                      = allVars s
allVars (AR (V s))                     = S.singleton s
allVars (AR (Neg s))                   = allVars (AR s)
  -- such repetition, much wow
allVars (BL (BBinary _ b1 b2))         = recurAndGet [BL b1, BL b2]
allVars (AR (ABinary _ a1 a2))         = recurAndGet [AR a1, AR a2]
allVars (If c t e)                     = recurAndGet [BL c, t, e]
allVars (While c e)                    = recurAndGet [BL c, e]
allVars (Seq xs)                       = recurAndGet xs
allVars _                              = mempty

-- | allVars helper function to map allVars and convert to Set
recurAndGet :: [Stmt] -> S.Set Var
recurAndGet = S.unions . fmap allVars

-- | given a variable, a function and the lineMap of the program, return the keys
-- that represents the statement that defined the variable after applying some
-- function f.
definedIn :: Var -> ([Int] -> [Int]) -> LineMap -> [Int]
definedIn var f lm = f . I.keys $ I.filter letDef lm
  where letDef (Let v _) = v == var
        letDef _         = False

-- | given a variable, and some unary f on lists, get the last line number that
-- the variable was defined at (the maximum line number in this case)
lastDefinedIn :: Var -> ([Int] -> [Int]) -> LineMap -> Int
lastDefinedIn var f lm = maximum . f . I.keys $ I.filter letDef lm
  where letDef (Let v _) = v == var
        letDef _         = False

-- | Transform a statement to a lineMap, keys are line numbers, values at stmts
toLineMap :: Stmt -> LineMap
toLineMap = I.fromList . flip tag 0

-- | node 1 takes all the edges from node 2, node 2's is removed
stealEdges :: Node -> Node -> LGraph -> LGraph
stealEdges n1 n2 g = g'
  where
    e2s = getEdge n2 g
    g' = alterEdge (nub . (++e2s)) n1 g

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
getDeps (i, s) = ask >>= \lm ->
                           return .
                           fmap (flip3 lastDefinedIn lm (filter (<i))) $
                           getDataDeps' s
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

-- | Find all control dependencies
getContDeps :: (Int, Stmt) -> [Node]
getContDeps (i, If b t e)  = fst <$> tail (tag (If b t e) i)
getContDeps (i, While b e) = fst <$> tail (tag (While b e) i)
getContDeps _         = []

-- | Given a node, and list of control dependencies, add those dependencies
addContDeps :: Node -> [Node] -> Engine LGraph LineMap LGraph
addContDeps _ [] = get
addContDeps node (d:ds) = do
        g <- get
        let newG = addEdgeWith (++) d [controlWrap node] g -- slow
        put newG
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
      
-- | Filter a graph based on a given variable, return the filtered graph
innerLoop :: LGraph -> LGraph -> LGraph
innerLoop g og = staticSlice newGraph og
  where 
    newGraph = foldEdges (\es acc -> foldr addNode' acc (unEdge <$> es)) g g
    getNodeEdge e = getEdge e og
    addNode' e = addNodeWEdge e (getNodeEdge e)

resolvedDeps :: LGraph -> Bool
resolvedDeps g = null $ edgeDeps L.\\ nodesInG
  where
    edgeDeps = nub (concatMap (\(x, y) -> x : (unEdge <$> y)) (edges g))
    nodesInG = nodes g

staticSlice :: LGraph -> LGraph -> LGraph
staticSlice g og  
  | resolvedDeps g = g
  | otherwise = innerLoop g og

-- | Given a graph, return a set of all the nodes that have control edges
contDefDeps :: LGraph -> S.Set Node
contDefDeps = S.fromList . nodes . filterEdges helper
  where
    helper :: [DEdge] -> Bool
    helper = any edgeTest

    edgeTest (Edge Control _) = True
    edgeTest _                = False

-- Sets are ordinal in haskell, this is actually a limitation, the list
-- implementation is way more complex and slow. Technically we do not want this
-- sorted, but it ends up working because of the nature of line numbers
toAST' :: LGraph -> S.Set Node
toAST' = S.fromList . nodes

-- | Given a graph and a lineMap, transform the graph into a Abstract Syntax Tree
-- wrapped in a Seq
toAST :: LGraph -> LineMap -> Stmt
toAST g lm = Seq . map (lm I.!) . S.toList $ wrapper controlDeps ast
  where controlDeps = contDefDeps g
        ast         = toAST' g
        wrapper cDeps a
          | S.null cDeps  = a
          | otherwise     = S.difference a controlDeps

-- | Test the conversion from AST to Data-Edge LGraph
tester1 :: Stmt -> LGraph
tester1 s = fst $ runEngine (toDCFG (tag s 0)) s

-- | Test the conversion from AST to Control-Edge LGraph
tester2 :: Stmt -> LGraph
tester2 s = fst $ runEngine (toCCFG (tag s 0)) s

-- | Test the conversion from AST to Data and Control Edge graph
tester :: Stmt -> LGraph
tester s = mconcat [tester1 s, tester2 s]

-- | Given an AST, and a variable, perform static slicing on that variable
sliceAndGraph :: Var -> Stmt -> LGraph
sliceAndGraph var x = staticSlice g og
  where defs = definedIn var id lm
        lm = toLineMap x
        og = tester x
        g = filterGraph (\n _ -> n `elem` defs) og

-- | Given an AST, and a variable, perform static slicing, and return a new AST
genAST :: Var -> Stmt -> Stmt
genAST var x = toAST (sliceAndGraph var x) (toLineMap x)
