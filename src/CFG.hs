module CFG where

import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Set as S
import Data.List.Extras (argmax)
import Data.List (nub)
-- import Control.Monad.State
-- import Debug.Trace (trace)

import Lang

-- | A node is a tuple of all the variables that that node depends on, and the
-- statement that constitutes the node
type Node = (Var, Stmt)

-- | Two Types of Edges, data dependence, control flow dependence
data Etype = Data
           | Control
           deriving (Show, Eq)

-- | An Edge is either a data edge or Control edge
data Edge t n = Edge t n
  deriving Show

-- TypeClass?
unEdge :: Edge t n -> n
unEdge (Edge _ n) = n

unTEdge :: Edge t n -> t
unTEdge (Edge t _) = t

type DEdge = Edge Etype Node

data MGraph n e = MGraph (M.Map n [e])
  deriving Show

type DGraph = MGraph Node DEdge

-- could avoid with newtype + record syntax
unGraph :: MGraph n e -> M.Map n [e]
unGraph (MGraph xs) = xs

class Graph g n e where
  graph :: [(n, [e])] -> g n e
  nodes :: g n e -> [n]
  edges :: g n e -> [(n, [e])]
  addNode :: n -> g n e -> g n e
  addEdge :: n -> e -> g n e -> g n e

class (Graph g n e) => CFG g n e where
  getDataDeps :: g n e -> [Node]
  getControlDeps :: g n e -> [Node]

instance (Ord n) => Graph MGraph n e where
  graph                   = MGraph . M.fromList
  nodes (MGraph xs)       = M.keys xs
  edges (MGraph es)       = M.toList es
  addNode n (MGraph es)   = MGraph $ M.insert n [] es
  addEdge n e (MGraph es) = MGraph $ M.insertWith (++) n [e] es

instance (Ord n) => Monoid (MGraph n e) where
  mempty = MGraph M.empty
  mappend (MGraph es) (MGraph es') = MGraph $ M.unionWith (++) es es'

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

tag :: Stmt -> Int -> [(Int, Stmt)]
tag s@(If _ t e) n = (n, s) : tag t (succ n) ++ tag e (succ n)
tag s@(While _ e) n = (n, s) : tag e (succ n)
tag (Seq xs) n = take (length xs) . concat $ pure tag <*> xs <*> [n..]
tag s n = [(succ n, s)]

-- | Given a statement, return all variables that are referenced inside that
-- statement
allVars :: Stmt -> S.Set Var
allVars (BL (RBinary _ (V s1) (V s2))) = S.fromList [s1, s2]
allVars (BL (Not b))                   = allVars (BL b)
allVars (BL (BBinary _ b1 b2))         = S.unions [ allVars (BL b1)
                                                  , allVars (BL b2)
                                                  ]
allVars (AR (V s))                     = S.singleton s
allVars (AR (ABinary _ a1 a2))         = S.unions [ allVars (AR a1)
                                                  , allVars (AR a2)
                                                  ]
allVars (Let _ s)                      = allVars s
allVars (If c t e)                     = S.unions [ allVars (BL c)
                                                  , allVars t
                                                  , allVars e
                                                  ]
allVars (While c e)                    = S.unions [ allVars (BL c)
                                                  , allVars e
                                                  ]
allVars (Seq xs)                       = S.unions $ fmap allVars xs
allVars _                              = mempty

-- | Given a Statement, pack the nodes in a graph with those statements
packNodes :: Stmt -> DGraph -> DGraph
packNodes a@(Let var _) g = addNode (var, a) g
packNodes (Seq ss)      g = mconcat $ flip packNodes g <$> ss
packNodes a             g = addNode ("", a) g

-- | Given a statement, and a graph, add data edges to that graph
toDataDCFG :: Stmt -> DGraph -> DGraph
toDataDCFG s@(Let var s')  g = mconcat $ helper <$> vars <*> ns
  where
    ns = nodes g
    vars = S.toList $ allVars s'
    helper x y = if x == fst y
               then addEdge y (Edge Data (var, s)) g
               else g

toDataDCFG s@(If c t e) g = mconcat $ [ toDataDCFG (BL c)
                                      , toDataDCFG t
                                      , toDataDCFG e
                                      ] <*> [g] ++ (helper <$> vars <*> ns) 
  where
    ns = nodes g
    vars = S.toList . S.unions $ [allVars (BL c), allVars t, allVars e]
    helper x y = if x == fst y
               then addEdge y (Edge Data ("", s)) g
               else g

toDataDCFG s@(While b e) g = mconcat $ [ toDataDCFG (BL b)
                                       , toDataDCFG e
                                       ] <*> [g] ++ (helper <$> vars <*> ns)
  where
    ns = nodes g
    vars = S.toList . S.unions $ [allVars (BL b), allVars e]
    helper x y = if x == fst y
               then addEdge y (Edge Data ("", s)) g
               else g
toDataDCFG (Seq ss) g = mconcat $ flip toDataDCFG g <$> ss
toDataDCFG _        g = g

-- | Given a statement, and a graph, add control flow edges
toDataCCFG :: Stmt -> DGraph -> DGraph
toDataCCFG s@(If _ t e)  g = addEdge tNode (Edge Control sNode) g `mappend`
                             addEdge eNode (Edge Control sNode) g
  where tNode = ("", t)
        eNode = ("", e)
        sNode = ("", s)
toDataCCFG s@(While _ e) g = addEdge eNode (Edge Control sNode) g
  where sNode = ("", s)
        eNode = ("", e)
toDataCCFG _             g = g

toCFG :: Stmt -> DGraph
toCFG s = mconcat $ [toDataCCFG s, toDataDCFG s] <*> pure (packNodes s mempty)

-- | Given a graph, return an AST
-- this is some sort of fold, also a CSP, with most constrained var strategy
toAST' :: DGraph -> [Stmt]
toAST' (MGraph es)
  | null l = []
  | otherwise = (snd . fst $ m) :
                toAST' (MGraph $ es `M.difference` M.fromList [m])
  where
    l = M.toList es
    m = argmax (length . snd) l

toAST :: DGraph -> Stmt
toAST es = Seq $ toAST' es

staticSlice :: Var -> DGraph -> DGraph
staticSlice var d@(MGraph ex) = ns `mappend` helper' (edgeDefs) d
  where defs = M.filterWithKey (\k _ -> fst k == var) ex
        edgeDefs = fmap unEdge .
                   filter ((==Data) . unTEdge) .
                   concat $ M.elems defs
        ns = graph . fmap (flip (,) []) . nub $ M.keys defs

-- The [Node] here is just an list of edges stripped of the their dependency type
helper' :: [Node] -> DGraph -> DGraph
helper' []      _          = mempty
helper' (x:xs) d@(MGraph ex) = (MGraph $ M.fromList [(x, [])]) `mappend`
                               helper' xs d `mappend` helper' esNodes d 
  where es = ex M.! x
        esNodes = fmap unEdge es
