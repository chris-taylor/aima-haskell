{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, ExplicitForAll #-}

module Search where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromJust)

import Queue

-- |The type used to represent the cost associated with a particular path.
type Cost = Double

-- |Class for an abstract problem with state type s and action type a. A
--  minimal implementation consists of 'initial' and 'successor', and one
--  of 'goal' or 'goalTest'.
class Eq s => Problem p s a where
    -- | The initial state of the problem.
    initial :: p s a -> s

    -- | Given a state, return a sequence of (action, state) pairs reachable
    --   from this state. Because of lazy evaluation we only ever compute as
    --   many elements of the list as the program needs.
    successor :: p s a -> s -> [(a, s)]
    
    -- | If the problem has a unique goal state, this method should return it.
    --   The default implementation of 'goalTest' compares for equality with
    --   this state.
    goal :: p s a -> s
    goal = undefined

    -- | Return true if the state is a goal. The default method compares the
    --   state to the state specified in the implementation of 'goal'. You can
    --   override this method if checking against a single goal is not enough.
    goalTest :: p s a -> s -> Bool
    goalTest p s = s == goal p

    -- | Return the cost of a solution path that arrives at the second state
    --   from the first state, via the specified action. If the problem is such
    --   that the path doesn't matter, the function will only look at the second
    --   state. The default implementation costs 1 for every step in the path.
    pathCost :: p s a -> Cost -> s -> a -> s -> Cost
    pathCost _ c _ _ _ = c + 1

-- |A node in a search tree. It contains a reference to its parent (the node
--  that this is a successor of) and to the state for this node. Note that if
--  a state can be arrived at by two paths, there will be two nodes with the
--  same state. It may also include the action that got us to this state, and
--  the total path cost.
data Node s a = Node { state  :: s
                     , parent :: Maybe (Node s a)
                     , action :: Maybe a
                     , cost   :: Cost
                     , depth  :: Int }

instance (Show s, Show a) => Show (Node s a) where
    show (Node state _ action cost depth) =
        "Node(state=" ++ show state ++ ",action=" ++ show action ++ 
            ",cost=" ++ show cost ++ ",depth=" ++ show depth ++ ")"

-- |A convenience constructor for root nodes (a node with no parent, no action
--  that leads to it, and zero cost and depth.)
root :: s -> Node s a
root s = Node s Nothing Nothing 0 0

-- |Create a list of paths from the root node to the node specified.
path :: Node s a -> [Node s a]
path n = case parent n of
    Nothing -> [n]
    Just n' -> n : path n'

-- |Return a list of nodes reachable from this node in the context of the
--  specified problem.
expand :: (Problem p s a) => p s a -> Node s a -> [Node s a]
expand p n = [ Node next (Just n) (Just act) (c act next) (depth n + 1) |
                (act,next) <- successor p (state n) ]
    where
        c act next = pathCost p (cost n) (state n) act next

----------------------------------
-- Uninformed Search Algorithms --
----------------------------------

-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. We don't worry about repeated paths
--  to a state.
treeSearch :: (Problem p s a, Queue q) => q (Node s a) -> p s a -> Maybe (Node s a)
treeSearch q problem = go problem (root (initial problem) `push` q)
    where
        go p fringe = if empty fringe
            then Nothing
            else let (node, rest) = pop fringe
                 in if goalTest p (state node)
                        then Just node
                        else go p (expand problem node `extend` rest)

-- |Search the deepest nodes in the search tree first.
depthFirstTreeSearch :: (Problem p s a) => p s a -> Maybe (Node s a)
depthFirstTreeSearch = treeSearch []

-- |Search the shallowest nodes in the search tree first.
breadthFirstTreeSearch :: (Problem p s a) => p s a -> Maybe (Node s a)
breadthFirstTreeSearch = treeSearch (FifoQueue [])

data DepthLimited a = Fail | Cutoff | Ok a deriving (Show)

-- |Depth-first search with a depth limit. If the depth limit is reached we
--  return 'Cutoff', otherwise return 'Fail' (if no solution is found) or 'Ok'
--  (if a solution is found) which take the place of Nothing and Just in the
--  other search functions.
depthLimitedSearch :: (Problem p s a) => Int -> p s a -> DepthLimited (Node s a)
depthLimitedSearch lim prob = recursiveDLS (root $ initial prob) prob lim
    where
        recursiveDLS node p lim
            | goalTest p (state node) = Ok node
            | depth node == lim       = Cutoff
            | otherwise               = filt False $ map go (expand prob node)
            where
                go node = recursiveDLS node p lim

                filt cutoff [] = if cutoff then Cutoff else Fail
                filt cutoff (Ok node : _)    = Ok node
                filt cutoff (Fail    : rest) = filt cutoff rest
                filt cutoff (Cutoff  : rest) = filt True   rest

-- |Repeatedly try depth-limited search with an increasing depth limit.
iterativeDeepeningSearch :: (Problem p s a) => p s a -> Maybe (Node s a)
iterativeDeepeningSearch prob = go 1
    where
        go lim = case depthLimitedSearch lim prob of
            Cutoff -> go (lim + 1)
            Fail   -> Nothing
            Ok n   -> Just n

---------------------------------
-- Informed (Heuristic) Search --
---------------------------------

bestFirstTreeSearch :: (Problem p s a) => (Node s a -> Double) -> p s a -> Maybe (Node s a)
bestFirstTreeSearch f = treeSearch (PQueue [] f)


--------------------
-- A test problem --
--------------------

data WP s a = WP { initialWP :: s, goalWP :: s, charsWP :: [a], maxLen :: Int } deriving (Show)

instance Problem WP String Char where
    initial = initialWP
    goal = goalWP
    --successor p s = [ (a, a:s) | a <- charsWP p ]
    successor p s = if length s == maxLen p
        then []
        else [ (a, a:s) | a <- charsWP p ]

wp :: WP String Char
wp = WP { initialWP = "", goalWP = "abracad", charsWP = "abrcd" , maxLen = 11 }

-------------------------------
-- Graphs and Graph Problems --
-------------------------------

data Graph a = G { getGraph :: (Map a [(a,Cost)]) } deriving (Show)

mkGraph :: Ord a => [(a,[(a,Cost)])] -> Graph a
mkGraph = G . M.fromList

costFromTo :: Ord a => Graph a -> a -> a -> Cost
costFromTo (G g) a b = case M.lookup a g of
    Nothing -> error "Vertex not found in graph!"
    Just ls -> case lookup b ls of
        Nothing -> 1/0
        Just c  -> c

data GraphProblem s a = GP { graphGP :: Graph s, initGP :: s, goalGP :: s } deriving (Show)

instance Ord s => Problem GraphProblem s s where
    initial = initGP
    goal = goalGP
    successor (GP (G g) _ _) s = [ (x,x) | (x,_) <- fromJust $ M.lookup s g ]
    pathCost (GP g _ _) c s _ s' = c + costFromTo g s s'

testGraph :: Graph Char
testGraph = mkGraph
    [ ('A', [('B',1), ('C',3)])
    , ('B', [('A',1), ('D',2)])
    , ('C', [('A',3), ('D',4)])
    , ('D', [('B',2), ('C',4)]) ]

gp :: GraphProblem Char Char
gp = GP { graphGP = testGraph, initGP = 'A', goalGP = 'D' }