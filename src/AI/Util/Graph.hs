module AI.Util.Graph
    ( Graph(..)
    , toGraph
    , toUndirectedGraph
    , getNodes
    , getNeighbours
    , getEdge
    , addEdge
    , addUndirectedEdge
    , UnweightedGraph(..)
    , parseGraph ) where

import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

---------------------
-- Weighted Graphs --
---------------------

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

-----------------------
-- Unweighted Graphs --
-----------------------

-- |Type for unweighted graphs
type UnweightedGraph a = Map a [a]

-- |Create a directed graph from an adjacency list.
toUnweightedGraph :: (Ord a) => [(a, [a])] -> UnweightedGraph a
toUnweightedGraph = M.fromList

-- |Create an undirected graph from an adjacency list.
toUnweightedUndirectedGraph :: Ord a => [(a, [a])] -> UnweightedGraph a
toUnweightedUndirectedGraph xs = fromPairRepUnweighted . symmetrizeUnweighted .
    toPairRepUnweighted $ toUnweightedGraph xs

-- |Convert an unweighted graph to its adjacency list representation.
fromGraphUnweighted :: UnweightedGraph a -> [(a, [a])]
fromGraphUnweighted = M.toList

-- |Convert an unweighted graph to its ordered pair representation.
toPairRepUnweighted :: UnweightedGraph a -> [(a,a)]
toPairRepUnweighted xs = [ (a,b) | (a,bs) <- fromGraphUnweighted xs, b <- bs ]

-- |Take an unweighted graph in ordered pair representation and add in all of
--  the reverse links, so that the resulting graph is directed.
symmetrizeUnweighted :: Eq a => [(a,a)] -> [(a,a)]
symmetrizeUnweighted xs = L.nub $ concat [ [(a,b),(b,a)] | (a,b) <- xs ]

-- |Convert an unweighted graph from its ordered pair representation.
fromPairRepUnweighted :: Ord a => [(a,a)] -> UnweightedGraph a
fromPairRepUnweighted xs = go xs M.empty
    where
        go []         m = m
        go ((a,b):xs) m = go xs (M.insert a newList m)
            where
                newList = b : case M.lookup a m of
                    Nothing -> []
                    Just l  -> l

-- |Parse an unweighted graph from a string. The string must be a semicolon-
--  separated list of associations between nodes and neighours. Each association
--  has the head node on the left, followed by a colon, followed by a list of
--  neighbours, for example:
--
--  > "A: B C; B: C D; C: D"
--
--  It is not necessary to specify reverse links - they will be added
--  automatically.
parseGraph :: String -> UnweightedGraph String
parseGraph str = toUnweightedUndirectedGraph $ textToStr $ splitNbrs $
                 parseNodes $ splitNodes $ T.pack str
    where
        splitNodes = map T.strip . T.split (== ';')
        parseNodes = map listToPair . map (T.split (== ':'))
        splitNbrs  = map (\(x,y) -> (x, T.words y))
        textToStr  = map (\(x,y) -> (T.unpack x, map T.unpack y))
        listToPair [x,y] = (x,y)