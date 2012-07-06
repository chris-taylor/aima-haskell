module AI.Util.Graph
    ( Graph(..)
    , toGraph
    , toUndirectedGraph
    , getNodes
    , getNeighbours
    , getEdge
    , addEdge
    , addUndirectedEdge ) where

import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.List as L

-- |A graph connects vertices (nodes) by edges (actions). Each edge can also 
--  have a weight associated with it. To build a graph, call one of the
--  functions 'toGraph' (for a directed graph) and 'toUndirectedGraph' (for
--  an undirected graph).
type Graph a b = Map a (Map a b)

-- |Create a directed graph from an adjacency list.
toGraph :: (Ord a) => [(a, [(a,b)])] -> Graph a b
toGraph xs = M.fromList (map f xs)
    where
        f (a,bs) = (a, M.fromList bs)

-- |Create an undirected graph from an adjacency list. The inverse links will
--  be added automatically.
toUndirectedGraph :: (Ord a, Eq b) => [(a,[(a,b)])] -> Graph a b
toUndirectedGraph conn = fromPairRep . symmetrize . toPairRep $ toGraph conn

-- |Get a list of the nodes of the graph.
getNodes :: Graph a b -> [a]
getNodes = M.keys

-- |Get a list of the outbound links from node @a@.
getNeighbours :: Ord a => a -> Graph a b -> [(a,b)]
getNeighbours a g = case M.lookup a g of
    Nothing -> error "Vertex not found in graph -- GETNEIGHBOURS"
    Just ls -> M.toList ls

-- |Get the weight attached to the edge between @x@ and @y@.
getEdge :: (Ord a) => a -> a -> Graph a b -> Maybe b
getEdge x y g = case M.lookup x g of
    Nothing -> error "Vertex not found in graph -- GETEDGE"
    Just ys -> M.lookup y ys

-- |Add an edge between two vertices to a Graph.
addEdge :: (Ord a) => a -> a -> b -> Graph a b -> Graph a b
addEdge x y e graph = let xs  = graph ! x
                          xs' = M.insert y e xs
                       in M.insert x xs' graph

-- |Add an undirected edge between two vertices to a Graph.
addUndirectedEdge :: (Ord a) => a -> a -> b -> Graph a b -> Graph a b
addUndirectedEdge x y e graph = addEdge y x e (addEdge x y e graph)

-- |Convert a graph to its adjacency list representation.
fromGraph :: Graph a b -> [(a, [(a,b)])]
fromGraph xs = map g (M.toList xs)
    where
        g (a,bs) = (a, M.toList bs)

-- |Convert a graph to its ordered pair representation.
toPairRep :: Graph a b -> [(a,a,b)]
toPairRep xs = [ (a,b,c) | (a,bs) <- fromGraph xs, (b,c) <- bs ]

-- |Take a directed graph in ordered pair representation and add in all of the
--  reverse links, so that the resulting graph is undirected.
symmetrize :: (Eq a, Eq b) => [(a,a,b)] -> [(a,a,b)]
symmetrize xs = L.nub $ concat [ [(a,b,c),(b,a,c)] | (a,b,c) <- xs ] 

-- |Convert a graph from its ordered pair representation.
fromPairRep :: (Ord a) => [(a,a,b)] -> Graph a b
fromPairRep xs = go xs M.empty
    where
        go []           m = m
        go ((a,b,c):xs) m = go xs (M.insert a newMap m)
            where
                newMap = M.insert b c $ case M.lookup a m of
                    Nothing -> M.empty
                    Just m' -> m'