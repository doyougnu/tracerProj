module CFG where

import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Set as S
import Control.Monad.Reader
-- import Control.Monad.Writer
import Control.Monad.State
import Data.List.Extras (argmax)
import Data.List (nub)
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
  deriving (Show, Eq)

-- TypeClass?
unEdge :: Edge t n -> n
unEdge (Edge _ n) = n

unTEdge :: Edge t n -> t
unTEdge (Edge t _) = t

-- | An edge in the Control Flow graph, is an edge with an edge type and the node
-- that the edge touches. For a map Map Node Edge the edge is assumed to be from
-- the key node to the node held in the edge type
type DEdge = Edge Etype Node

-- | abstract representation of a Graph constructed by an adjacency Map.
data MGraph n e = MGraph (M.Map n [e])
  deriving Show

-- | type synonym to construct our actual type
type DGraph = MGraph Node DEdge

-- | An integer map where keys are line numbers, values are statements.
type LineMap = I.IntMap Stmt
type NNode = Int
type LGraph = MGraph NNode (Edge Etype NNode)

-- could avoid with newtype + record syntax
unGraph :: MGraph n e -> M.Map n [e]
unGraph (MGraph xs) = xs

class Graph g n e where
  graph :: [(n, [e])] -> g n e
  nodes :: g n e -> [n]
  edges :: g n e -> [(n, [e])]
  addNode :: n -> g n e -> g n e
  addEdge :: n -> e -> g n e -> g n e
  getEdges :: n -> g n e -> [e]
  alterEdge :: ([e] -> [e]) -> n -> g n e -> g n e
  getNodes :: e -> g n e -> [n]
  removeEdge :: n -> e -> g n e -> g n e
  removeNode :: n -> g n e -> g n e

class (Graph g n e) => CFG g n e where
  getDataDeps    :: g n e -> [Node]
  getControlDeps :: g n e -> [Node]

instance (Ord n, Eq e) => Graph MGraph n e where
  graph                      = MGraph . M.fromList
  nodes (MGraph xs)          = M.keys xs
  edges (MGraph es)          = M.toList es
  addNode n (MGraph es)      = MGraph $ M.insert n [] es
  addEdge n e (MGraph es)    = MGraph $ M.insertWith (++) n [e] es
  getEdges n (MGraph es)     = es M.! n --exception if node not in map
  alterEdge f n (MGraph es)  = MGraph $ M.adjust f n es
  getNodes x (MGraph es)     = M.keys $ M.filter (elem x) es 
  removeEdge k e (MGraph es) = MGraph $ M.adjust (filter (/= e)) k es
  removeNode k (MGraph es)   = MGraph $ M.delete k es

instance (Ord n) => Monoid (MGraph n e) where
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
tag :: Stmt -> Int -> [(Int, Stmt)]
tag s@(If _ t e) n = (n, s) : tag t (succ n) ++ tag e (succ n)
tag s@(While _ e) n = (n, s) : tag e (succ n)
-- can't get this fold to work right
-- tag (Seq xs) n = foldr ((++) . flip tag n) [] xs
tag (Seq (x:xs)) n = tag x n ++ tag (Seq xs) (succ n)
tag s n = [(succ n, s)]

-- | Simple helper function passes the stmt to tag with an incremented n
incAndRecur :: Stmt -> Int -> [(Int, Stmt)] 
incAndRecur s n= tag s (succ n) 

-- | Given a statement, lift everything to statement level
-- decompose :: Stmt -> [Stmt]
-- decompose (Let _ s')             = decompose s'
-- decompose (If b c t)             = [BL b, c, t]
-- decompose (While b e)            = [BL b, e]
-- decompose (Seq xs)               = xs >>= decompose
-- decompose (BL (RBinary _ x y))   = [AR x, AR y]
-- decompose (BL (BBinary _ b1 b2)) = [BL b1, BL b2]
-- decompose (BL (Not b))           = [BL b]
-- decompose (AR (ABinary _ a1 a2)) = [AR a1, AR a2]
-- decompose (AR (V var))           = [AR $ V var]
-- decompose _                      = []

-- | Given a statement, return all variables that are referenced inside that
-- statement
-- allVars :: Stmt -> S.Set Var

allVars :: Stmt -> S.Set Var
allVars (BL (RBinary _ (V s1) (V s2))) = S.fromList [s1, s2]
allVars (BL (Not b))                   = allVars (BL b)
allVars (Let _ s)                      = allVars s
allVars (AR (V s))                     = S.singleton s
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
  
-- | Given a Statement, pack the nodes in a graph with those statements
-- packNodes :: Stmt -> DGraph -> DGraph
-- packNodes a@(Let var _) g = addNode (var, a) g
-- packNodes (Seq ss)      g = mconcat $ flip packNodes g <$> ss
-- packNodes a             g = addNode ("", a) g

definedIn :: Var -> LineMap -> [Int]
definedIn var lm = I.keys $ I.filter letDef lm
  where letDef (Let v _) = v == var
        letDef _         = False

toLineMap :: Stmt -> LineMap
toLineMap = I.fromList . flip tag 0 

isLetDef :: String -> Stmt -> Bool
isLetDef var (Let v _) = var == v
isLetDef _   _         = False

-- | node 1 takes all the edges from node 2, node 2's is removed
stealEdges :: NNode -> NNode -> LGraph -> LGraph
stealEdges n1 n2 g = g''
  where 
    e2s = getEdges n2 g
    g' = alterEdge (nub . (++e2s)) n1 g
    g'' = removeNode n2 g'

-- | Given a statement, and a graph, add data edges to that graph
type Engine s r a = StateT s (Reader r) a

-- toDataDCFG :: Stmt -> LineMap -> LGraph -> LGraph
-- toDataDCFG :: Stmt -> Engine LGraph LineMap ()
-- toDataDCFG s@(Let var s') = do
--   lm <- ask
--   g <- get
--   let ns = nodes g
--   let vars = S.toList $ allVars s'
--   if null $ getKey s g
--     then addNode $ getKey s lm
--     else 
  -- check if var has been defined before
      -- if it has then steal edges, then remove edges
      -- if it has not then add with no edges
  -- Check all vars for all nodes, if those vars are referenced add a data edge
-- toDataDCFG s@(If c t e) g = mconcat $ toDataDCFG <$>
--                             [BL c, t , e] <*> g : (helper <$> vars <*> ns) 
--   where
--     ns = nodes g
--     vars = S.toList . S.unions $ [allVars (BL c), allVars t, allVars e]
--     helper x y = if x == fst y
--                  then addEdge y (Edge Data ("", s)) g
--                  else g

-- toDataDCFG s@(While b e) g = mconcat $ [ toDataDCFG (BL b)
--                                        , toDataDCFG e
--                                        ] <*> g : (helper <$> vars <*> ns)
--   where
--     ns = nodes g
--     vars = S.toList . S.unions $ [allVars (BL b), allVars e]
--     helper x y = if x == fst y
--                  then addEdge y (Edge Data ("", s)) g
--                  else g
-- toDataDCFG (Seq ss) g = mconcat $ flip toDataDCFG g <$> ss
-- toDataDCFG _        g = g

-- toDataDCFG :: Stmt -> LineMap -> LGraph -> DGraph
-- toDataDCFG n lm g =
--   where ns = nodes g

-- | Given a statement, and a graph, add control flow edges
-- toDataCCFG :: Stmt -> DGraph -> DGraph
-- toDataCCFG s@(If _ t e)  g = addEdge tNode (Edge Control sNode) g `mappend`
--                              addEdge eNode (Edge Control sNode) g
--   where tNode = ("", t)
--         eNode = ("", e)
--         sNode = ("", s)
-- toDataCCFG s@(While _ e) g = addEdge eNode (Edge Control sNode) g
--   where sNode = ("", s)
--         eNode = ("", e)
-- toDataCCFG _             g = g

-- toCFG :: Stmt -> DGraph
-- toCFG s = mconcat $ [toDataCCFG s, toDataDCFG s] <*> pure (packNodes s mempty)

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

-- staticSlice :: Var -> DGraph -> DGraph
-- staticSlice var d@(MGraph ex) = ns `mappend` helper' (edgeDefs) d
--   where defs = M.filterWithKey (\k _ -> fst k == var) ex
--         edgeDefs = fmap unEdge .
--                    filter ((==Data) . unTEdge) .
--                    concat $ M.elems defs
--         ns = graph . fmap (flip (,) []) . nub $ M.keys defs

-- The [Node] here is just an list of edges stripped of the their dependency type
helper' :: [Node] -> DGraph -> DGraph
helper' []      _          = mempty
helper' (x:xs) d@(MGraph ex) = (MGraph $ M.fromList [(x, [])]) `mappend`
                               helper' xs d `mappend` helper' esNodes d 
  where es = ex M.! x
        esNodes = fmap unEdge es
