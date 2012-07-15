{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module AI.Search.Search where

import Data.Set (Set)
import qualified Data.Set as S

import AI.Util.Queue
import AI.Util.Util

--import qualified AI.Util.Graph as G

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
    costP :: p s a -> Cost -> s -> a -> s -> Cost
    costP _ c _ _ _ = c + 1

    -- | You may want to specify a heuristic function for the problem. The
    --   default implementation always returns zero.
    heuristic :: p s a -> Node s a -> Cost
    heuristic _ = const 0

    -- | For optimization problems, each state has a value. Hill-climbing and
    --   related algorithms try to maximise this value. The default
    --  implementation always returns zero.
    valueP :: p s a -> s -> Double
    valueP _ = const 0

-- |A node in a search tree. It contains a reference to its parent (the node
--  that this is a successor of) and to the state for this node. Note that if
--  a state can be arrived at by two paths, there will be two nodes with the
--  same state. It may also include the action that got us to this state, and
--  the total path cost.
data Node s a = Node { state  :: s
                     , parent :: Maybe (Node s a)
                     , action :: Maybe a
                     , cost   :: Cost
                     , depth  :: Int
                     , value  :: Double }

instance (Show s, Show a) => Show (Node s a) where
    show (Node state _ action cost depth _) =
        "Node(state=" ++ show state ++ ",action=" ++ show action ++ 
            ",cost=" ++ show cost ++ ",depth=" ++ show depth ++ ")"

-- |A convenience constructor for root nodes (a node with no parent, no action
--  that leads to it, and zero cost.)
root :: (Problem p s a) => p s a -> Node s a
root p = Node s Nothing Nothing 0 0 (valueP p s) where s = initial p

-- |Create a list of paths from the root node to the node specified.
path :: Node s a -> [Node s a]
path n = case parent n of
    Nothing -> [n]
    Just n' -> n : path n'

-- |Return a list of nodes reachable from this node in the context of the
--  specified problem.
expand :: (Problem p s a) => p s a -> Node s a -> [Node s a]
expand p node = [ mkNode a s | (a,s) <- successor p (state node) ]
    where
        mkNode a s = Node s (Just node) (Just a) (c a s) (1 + depth node) v
        c      a s = costP p (cost node) (state node) a s
        v          = valueP p (state node)

----------------------------------
-- Uninformed Search Algorithms --
----------------------------------

-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. We don't worry about repeated paths
--  to a state.
treeSearch :: (Problem p s a, Queue q) =>
              q (Node s a)      -- ^ Empty queue
           -> p s a             -- ^ Problem
           -> Maybe (Node s a)
treeSearch q prob = go prob (root prob `push` q)
    where
        go p fringe = if empty fringe
            then Nothing
            else let (node, rest) = pop fringe
                 in if goalTest p (state node)
                    then Just node
                    else go p (expand prob node `extend` rest)

-- |Search the deepest nodes in the search tree first.
depthFirstTreeSearch :: (Problem p s a) => p s a -> Maybe (Node s a)
depthFirstTreeSearch = treeSearch []

-- |Search the shallowest nodes in the search tree first.
breadthFirstTreeSearch :: (Problem p s a) => p s a -> Maybe (Node s a)
breadthFirstTreeSearch = treeSearch (newQueue :: FifoQueue (Node s a))

-- |Search through the successors of a node to find a goal. The argument
--  @fringe@ should be an empty queue. If two paths reach the same state, use
--  only the best one.
graphSearch :: (Problem p s a, Queue q, Ord s) =>
               q (Node s a)     -- ^ Empty queue
            -> p s a            -- ^ Problem
            -> Maybe (Node s a)
graphSearch q prob = go prob (root prob `push` q) S.empty
    where
        go p fringe closed = if empty fringe
            then Nothing
            else if goalTest p thisState
                then Just node
                else if thisState `S.member` closed
                    then go p rest  closed
                    else go p rest' closed'
            where
                (node,rest) = pop fringe
                thisState   = state node
                rest'       = expand prob node `extend` rest
                closed'     = thisState `S.insert` closed

-- |Search the deepest nodes in the graph first.
depthFirstGraphSearch :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
depthFirstGraphSearch = graphSearch []

-- |Search the shallowest nodes in the graph first.
breadthFirstGraphSearch :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
breadthFirstGraphSearch = graphSearch (newQueue :: FifoQueue (Node s a))

-- |Depth-first search with a depth limit. If the depth limit is reached we
--  return 'Cutoff', otherwise return 'Fail' (if no solution is found) or 'Ok'
--  (if a solution is found) which take the place of Nothing and Just in the
--  other search functions.
depthLimitedSearch :: (Problem p s a) =>
                      Int       -- ^ Depth limit
                   -> p s a     -- ^ Problem
                   -> DepthLimited (Node s a)
depthLimitedSearch lim prob = recursiveDLS (root prob) prob lim
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

data DepthLimited a = Fail | Cutoff | Ok a deriving (Show)

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

-- |Best-first tree search takes a function that scores each potential successor
--  and prefers to explore nodes with the lowest score first.
bestFirstTreeSearch :: (Problem p s a) =>
                       (Node s a -> Double) -- ^ Function to score each node
                    -> p s a                -- ^ Problem
                    -> Maybe (Node s a)
bestFirstTreeSearch f = treeSearch (PQueue [] f)

-- |Best-first graph search keeps track of states that have already been visited
--  and won't visit the same state twice.
bestFirstGraphSearch :: (Problem p s a, Ord s) =>
                        (Node s a -> Double)    -- ^ Function to score each node
                     -> p s a                   -- ^ Problem
                     -> Maybe (Node s a)
bestFirstGraphSearch f = graphSearch (PQueue [] f)

-- |Minimum cost search preferentially explores nodes with the lowest cost
--  accrued, to guarantee that it finds the best path to the solution.
uniformCostSearch :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
uniformCostSearch prob = bestFirstGraphSearch cost prob

-- |Greedy best-first search preferentially explores nodes with the lowest
--  cost remaining to the goal, ignoring cost already accrued.
greedyBestFirstSearch :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
greedyBestFirstSearch prob = bestFirstGraphSearch (heuristic prob) prob

-- |A* search takes a heuristic function that estimates how close each state is
--  to the goal. It combines this with the path cost so far to get a total
--  score, and preferentially explores nodes with a lower score. It is optimal
--  whenever the heuristic function is 
aStarSearch :: (Problem p s a, Ord s) =>
               (Node s a -> Double)         -- ^ Heuristic function
            -> p s a                        -- ^ Problem
            -> Maybe (Node s a)
aStarSearch h = bestFirstGraphSearch (\n -> h n + cost n)

-- |A variant on A* search that uses the heuristic function defined by the
--  problem.
aStarSearch' :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
aStarSearch' prob = aStarSearch (heuristic prob) prob

-----------------------------
-- Local Search Algorithms --
-----------------------------

-- |From the initial node, keep choosing the neighbour with the highest value,
--  stopping when no neighbour is better.
hillClimbingSearch :: (Problem p s a) => p s a -> Node s a
hillClimbingSearch prob = go (root prob)
    where
        go node = if value neighbour <= value node
            then node
            else go neighbour
            where
                neighbour = argMax (expand prob node) value

-- |Data type for an annealing schedule.
type Schedule = Int -> Double

-- |One possible schedule function for simulated annealing.
expSchedule :: Double -> Int -> Schedule
expSchedule lambda limit k = if k < limit
    then exp (-lambda * fromIntegral k)
    else 0

-- |Simulated annealing search. At each stage a random neighbour node is picked,
--  and we move to that node if its value is higher than the current
--  node. If its value is lower, then we move to it with some probability
--  depending on the current 'temperature'. The temperature is gradually
--  reduced according to an annealing schedule, making random jumps less likely
--  as the algorithm progresses.
simulatedAnnealing :: (Problem p s a) => Schedule -> p s a -> IO (Node s a)
simulatedAnnealing schedule prob = go 0 (root prob)
    where
        go k current = let t = schedule k in
            if t == 0
                then return current
                else do
                    next <- randomChoiceIO (expand prob current)
                    let deltaE = value next - value current
                    jump <- probabilityIO (exp $ deltaE / t)
                    if deltaE > 0 || jump
                        then go (k+1) next
                        else go (k+1) current
