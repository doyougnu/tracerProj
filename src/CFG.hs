module CFG where

import Data.Map as M
import Data.Set as S

import Lang

-- | Nodes will be statements in the language
type Node = Stmt

-- | Two Types of Edges, data dependence, control flow dependence
data Etype = Data
           | Control

data Edge = Edge Etype Node Node 

data DGraph n e = DGraph (S.Set n) (M.Map n [e])

class Graph g n e where
  nodes :: g n e -> [n]
  edges :: g n e -> [(n, [e])]
  addNode :: n -> g n e -> g n e
  addEdge :: n -> e -> g n e -> g n e

class (Graph g n e) => CFG g n e where
  getDataDeps :: g n e -> [Node]
  getControlDeps :: g n e -> [Node]

instance Graph DGraph Node Edge where
  nodes (DGraph ns _) = S.toList ns
  edges (DGraph _ es) = M.toList es
  addNode n (DGraph ns es) = DGraph (S.insert n ns) es
  addEdge n e (DGraph ns es) = DGraph ns (M.insertWith (++) n [e] es)

toCFG :: Stmt -> DGraph Node Edge -> DGraph Node Edge
toCFG s@(BL boolExpr) g = addNode s g
toCFG s@(AR arExpr)   g = addNode s g
toCFG s@(ST str)      g = addNode s g
toCFG s@(Let var s')  g = g''
  where g' = addNode s g
        g'' = if mentions var s'
              then addEdge s (Edge Data s s') g'
              else g'
-- toCFG s@(If c t e)    g = 

mentions :: Var -> Stmt -> Bool
mentions str (BL (RBinary _ (V s1) (V s2))) = s1 == str || s2 == str
mentions str (AR (V s)) = str == s
mentions str (AR (ABinary _ a1 a2)) = mentions str (AR a1) || mentions str (AR a2)
mentions str (Let _ s) = mentions str s
mentions str (If c t e) = mentions str (BL c) || mentions str t || mentions str e
mentions str (While c e) = mentions str (BL c) || mentions str e
mentions str (Seq xs) = any (mentions str) xs
mentions _   _        = False

allVars :: Stmt -> S.Set Var
allVars (BL (RBinary _ (V s1) (V s2))) = S.fromList [s1, s2]
allVars (BL (Not b)) = allVars (BL b)
allVars (BL (BBinary _ b1 b2)) = S.unions [allVars (BL b1), allVars (BL b2)]
allVars (AR (V s)) = S.singleton s
allVars (AR (ABinary _ a1 a2)) = S.unions [allVars (AR a1), allVars (AR a2)]
allVars (Let _ s) = allVars s
allVars (If c t e) = S.unions [allVars (BL c), allVars t, allVars e]
allVars (While c e) = S.unions [allVars (BL c), allVars e]
allVars (Seq xs) =  S.unions $ fmap allVars xs
allVars _        = S.empty
