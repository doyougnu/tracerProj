module CFG where

import qualified Data.Map as M
import qualified Data.Set as S
-- import Control.Arrow
-- import Debug.Trace (trace)

import Lang

-- | A node is a tuple of all the variables that that node depends on, and the
-- statement that constitutes the node
type Node = (Var, Stmt)

-- | Two Types of Edges, data dependence, control flow dependence
data Etype = Data
           | Control
           deriving Show

-- | An Edge is either a data edge or Control edge
data Edge = Edge Etype Node Node 
  deriving Show

data DGraph n e = DGraph (S.Set n) (M.Map n [e])
  deriving Show

class Graph g n e where
  nodes :: g n e -> [n]
  edges :: g n e -> [(n, [e])]
  addNode :: n -> g n e -> g n e
  addEdge :: n -> e -> g n e -> g n e

class (Graph g n e) => CFG g n e where
  getDataDeps :: g n e -> [Node]
  getControlDeps :: g n e -> [Node]

instance Graph DGraph Node Edge where
  nodes (DGraph ns _)        = S.toList ns
  edges (DGraph _ es)        = M.toList es
  addNode n (DGraph ns es)   = DGraph (S.insert n ns) es
  addEdge n e (DGraph ns es) = DGraph ns (M.insertWith (++) n [e] es)

instance Monoid (DGraph Node Edge) where
  mempty = DGraph S.empty M.empty
  mappend (DGraph ns es) (DGraph ns' es') =
    DGraph (S.union ns ns') (M.unionWith (++) es es')

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

-- | Given a Statement, pack the nodes in a graph with those statments
packNodes :: Stmt -> DGraph Node Edge -> DGraph Node Edge
packNodes a@(Let var _) g = addNode (var, a) g
packNodes (Seq ss)      g = mconcat $ flip packNodes g <$> ss
packNodes a             g = addNode ("", a) g

-- | Given a statement, and a graph, add data edges to that graph
toDataDCFG :: Stmt -> DGraph Node Edge -> DGraph Node Edge
toDataDCFG s@(Let var s')  g = mconcat $ helper <$> vars <*> ns
  where
    ns = nodes g
    vars = S.toList $ allVars s'
    helper x y = if x == fst y
               then addEdge y (Edge Data y (var, s)) g
               else g

toDataDCFG s@(If c t e) g = mconcat $ toDataDCFG (BL c) g :
                            toDataDCFG t g :
                            toDataDCFG e g :
                            (helper <$> vars <*> ns) 
  where
    ns = nodes g
    vars = S.toList . S.unions $ [allVars (BL c), allVars t, allVars e]
    helper x y = if x == fst y
               then addEdge y (Edge Data y ("", s)) g
               else g

toDataDCFG s@(While b e) g = mconcat $ toDataDCFG (BL b) g :
                             toDataDCFG e g :
                             (helper <$> vars <*> ns)
  where
    ns = nodes g
    vars = S.toList . S.unions $ [allVars (BL b), allVars e]
    helper x y = if x == fst y
               then addEdge y (Edge Data y ("", s)) g
               else g
toDataDCFG (Seq ss) g = mconcat $ flip toDataDCFG g <$> ss
toDataDCFG _        g = g

-- | Given a statement, and a graph, add control flow edges
toDataCCFG :: Stmt -> DGraph Node Edge -> DGraph Node Edge
toDataCCFG s@(If c t e)  g = addEdge tNode (Edge Control sNode tNode) g `mappend`
                             addEdge eNode (Edge Control sNode eNode) g
  where tNode = ("", t)
        eNode = ("", e)
        sNode = ("", s)
toDataCCFG s@(While b e) g = addEdge eNode (Edge Control sNode eNode) g
  where sNode = ("", s)
        eNode = ("", e)
toDataCCFG _             g = g
