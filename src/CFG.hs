module CFG where

import Data.Map as M
import Data.Set as S

import Lang

-- | Nodes will be statements in the language
type Node = Stmt

-- | Two Types of Edges, data dependence, control flow dependence
data Etype = Data
           | Control

data Edge n = Etype Node Node

data DGraph n e = DGraph (S.Set n) (M.Map n [e])

class Graph g n e where
  nodes :: g n e -> [n]
  edges :: g n e -> [(n, [e])]
  addNode :: n -> g n e -> g n e
  addEdge :: n -> e -> g n e -> g n e

class (Graph g n e) => CFG g n e where
  getDataDeps :: g n e -> [Node]
  getControlDeps :: g n e -> [Node]

instance Graph DGraph Node (Edge Stmt) where
  nodes (DGraph ns _) = S.toList ns
  edges (DGraph _ es) = M.toList es
  addNode n (DGraph ns es) = DGraph (S.insert n ns) es
  addEdge n e (DGraph ns es) = DGraph ns (M.insertWith (++) n [e] es)
