{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module AI.Search where

import Control.Monad
import Data.IORef
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set (Set)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import AI.Util.Graph (Graph)
import AI.Util.Queue
import AI.Util.Table
import AI.Util.Util

import qualified AI.Util.Graph as G

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
    --  implementation always returns Nothing.
    valueP :: p s a -> s -> Maybe Double
    valueP _ = const Nothing

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
                     , value  :: Maybe Double }

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
breadthFirstTreeSearch = treeSearch (FifoQueue [])

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
breadthFirstGraphSearch = graphSearch (FifoQueue [])

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
minimumCostSearch :: (Problem p s a, Ord s) => p s a -> Maybe (Node s a)
minimumCostSearch prob = bestFirstGraphSearch cost prob

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

-------------------------------
-- Graphs and Graph Problems --
-------------------------------

data GraphMap a = G
    { getGraph     :: Graph a Cost
    , getLocations :: Map a Point } deriving (Show)

type Point = (Double,Double)

mkGraphMap :: (Ord a) => [(a,[(a,Cost)])] -> [(a,Point)] -> GraphMap a
mkGraphMap conn loc = G (G.toUndirectedGraph conn) (M.fromList loc)

getNeighbors :: Ord a => a -> GraphMap a -> [(a,Cost)]
getNeighbors a (G g _) = G.getNeighbours a g

getLocation :: Ord a => a -> GraphMap a -> Point
getLocation a (G _ l) = case M.lookup a l of
    Nothing -> error "Vertex not found in graph -- GETLOCATION"
    Just pt -> pt

costFromTo :: Ord a => GraphMap a -> a -> a -> Cost
costFromTo graph a b = case lookup b (getNeighbors a graph) of
    Nothing -> 1/0
    Just c  -> c

data GraphProblem s a = GP
    { graphGP :: GraphMap s
    , initGP :: s
    , goalGP :: s } deriving (Show)

instance Ord s => Problem GraphProblem s s where
    initial = initGP
    goal = goalGP
    successor (GP g _ _) s = [ (x,x) | (x,_) <- getNeighbors s g ]
    costP (GP g _ _) c s _ s' = c + costFromTo g s s'
    heuristic (GP g _ goal) n = euclideanDist x y
        where
            x = getLocation (state n) g
            y = getLocation goal g

euclideanDist :: Point -> Point -> Double
euclideanDist (x,y) (x',y') = sqrt $ (x-x')^2 + (y-y')^2

romania :: GraphMap String
romania = mkGraphMap

    [ ("A", [("Z",75), ("S",140), ("T",118)])
    , ("B", [("U",85), ("P",101), ("G",90), ("F",211)])
    , ("C", [("D",120), ("R",146), ("P",138)])
    , ("D", [("M",75)])
    , ("E", [("H",86)])
    , ("F", [("S",99)])
    , ("H", [("U",98)])
    , ("I", [("V",92), ("N",87)])
    , ("L", [("T",111), ("M",70)])
    , ("O", [("Z",71), ("S",151)])
    , ("P", [("R",97)])
    , ("R", [("S",80)])
    , ("U", [("V",142)]) ]

    [ ("A",( 91,491)), ("B",(400,327)), ("C",(253,288)), ("D",(165,299))
    , ("E",(562,293)), ("F",(305,449)), ("G",(375,270)), ("H",(534,350))
    , ("I",(473,506)), ("L",(165,379)), ("M",(168,339)), ("N",(406,537))
    , ("O",(131,571)), ("P",(320,368)), ("R",(233,410)), ("S",(207,457))
    , ("T",( 94,410)), ("U",(456,350)), ("V",(509,444)), ("Z",(108,531)) ]

australia :: GraphMap String
australia = mkGraphMap

    [ ("T",   [])
    , ("SA",  [("WA",1), ("NT",1), ("Q",1), ("NSW",1), ("V",1)])
    , ("NT",  [("WA",1), ("Q",1)])
    , ("NSW", [("Q", 1), ("V",1)]) ]

    [ ("WA",(120,24)), ("NT" ,(135,20)), ("SA",(135,30)),
      ("Q" ,(145,20)), ("NSW",(145,32)), ("T" ,(145,42)), ("V",(145,37))]

gp1, gp2, gp3  :: GraphProblem String String

gp1 = GP { graphGP = australia, initGP = "Q", goalGP = "WA" }
gp2 = GP { graphGP = romania, initGP = "A", goalGP = "B" }
gp3 = GP { graphGP = romania, initGP = "O", goalGP = "N" }

----------------------
-- N Queens Problem --
----------------------

data NQueens s a = NQ { sizeNQ :: Int } deriving (Show)

-- |Update the state of the N-Queens board by playing a queen at (i,n).
updateNQ :: (Int,Int) -> [Maybe Int] -> [Maybe Int]
updateNQ (c,r) s = insert c (Just r) s

-- |Would putting two queens in (r1,c1) and (r2,c2) conflict?
conflict :: Int -> Int -> Int -> Int -> Bool
conflict r1 c1 r2 c2 =
    r1 == r2 || c1 == c2 || r1-c1 == r2-c2 || r1+c1 == r2+c2

-- |Would placing a queen at (row,col) conflict with anything?
conflicted :: [Maybe Int] -> Int -> Int -> Bool
conflicted state row col = or $ map f (enumerate state)
    where
        f (_, Nothing) = False
        f (c, Just r)  = if c == col && r == row
            then False
            else conflict row col r c

instance Problem NQueens [Maybe Int] (Int,Int) where
    initial (NQ n) = replicate n Nothing

    -- @L.elemIndex Nothing s@ finds the index of the first column in s
    -- that doesn't yet have a queen.
    successor (NQ n) s = case L.elemIndex Nothing s of
        Nothing -> []
        Just i  -> zip actions (map (`updateNQ` s) actions)
            where
                actions = map ((,) i) [0..n-1]

    goalTest (NQ n) s = if last s == Nothing
        then False
        else not . or $ map f (enumerate s)
            where
                f (c,Nothing) = False
                f (c,Just r)  = conflicted s r c

p :: NQueens [Maybe Int] (Int,Int)
p = NQ 8

-----------------------
-- Compare Searchers --
-----------------------

-- |Wrapper for a problem that keeps statistics on how many times nodes were
--  expanded in the course of a search. 
data ProblemIO p s a = PIO
    { problemIO     :: p s a
    , numGoalChecks :: IORef Int
    , numSuccs      :: IORef Int
    , numStates     :: IORef Int }

mkProblemIO :: p s a -> IO (ProblemIO p s a)
mkProblemIO p = do
    i <- newIORef 0
    j <- newIORef 0
    k <- newIORef 0
    return (PIO p i j k)

instance (Problem p s a, Eq s, Show s) => Problem (ProblemIO p) s a where
    initial (PIO p _ _ _) = initial p

    goalTest (PIO p n _ _) s = unsafePerformIO $ do
        modifyIORef n (+1)
        return (goalTest p s)

    successor (PIO p _ n m) s = unsafePerformIO $ do
        let succs = successor p s
        modifyIORef n (+1)
        modifyIORef m (+length succs)
        return succs

    costP (PIO p _ _ _) = costP p

    heuristic (PIO p _ _ _) = heuristic p

testSearcher :: p s a -> (ProblemIO p s a -> t) -> IO (t,Int,Int,Int)
testSearcher prob searcher = do
    p@(PIO _ numGoalChecks numSuccs numStates) <- mkProblemIO prob
    let result = searcher p in result `seq` do
        i <- readIORef numGoalChecks
        j <- readIORef numSuccs
        k <- readIORef numStates
        return (result, i, j, k)

testSearchers :: [ProblemIO p s a -> t] -> p s a -> IO [(t,Int,Int,Int)]
testSearchers searchers prob = testSearcher prob `mapM` searchers

compareSearchers :: (Show t) => [ProblemIO p s a -> t]
                 -> [p s a]
                 -> [String]
                 -> [String]
                 -> IO [[(t,Int,Int,Int)]] 
compareSearchers searchers probs header rownames = do
    results <- testSearchers searchers `mapM` probs
    printTable 20 (map (map f) results) header rownames
    return results
    where
        f (x,i,j,k) = (i,j,k)

detailedCompareSearchers ::
        [ProblemIO p s a -> Maybe (Node s1 a1)]
     -> [String]
     -> p s a
     -> IO ()
detailedCompareSearchers searchers names prob = do
    result <- testSearchers searchers prob
    table  <- forM result $ \(n,numGoalChecks,numSuccs,numStates) -> do
        let d = depth $ fromJust n
        let c = round $ cost $ fromJust n
        return [d,c,numGoalChecks,numSuccs,numStates]
    printTable 15 table header names
    where
        header = ["Searcher","Depth","Cost","Goal Checks","Successors",
                  "States"]

compareGraphSearchers :: IO ()
compareGraphSearchers = do
    compareSearchers searchers probs header rownames
    return ()
    where
        searchers = [ breadthFirstTreeSearch, breadthFirstGraphSearch
                    , depthFirstGraphSearch, iterativeDeepeningSearch
                    , minimumCostSearch, greedyBestFirstSearch, aStarSearch']
        probs = [gp1, gp2, gp3]
        rownames = ["Romania(A,B)","Romania(O,N)","Australia"]
        header = [ "Problem", "Breadth First Tree Search"
                 , "Breadth First Graph Search", "Depth First Graph Search"
                 , "Iterative Deepening Search", "MinimumCostSearch"
                 , "GreedyBestFirstSearch", "A* Search"]

runDetailedCompare :: (Problem p s a, Ord s, Show s) => p s a -> IO ()
runDetailedCompare = detailedCompareSearchers allSearchers allSearcherNames

allSearchers :: (Problem p s a, Ord s) => [p s a -> Maybe (Node s a)]
allSearchers = [ breadthFirstTreeSearch, breadthFirstGraphSearch
               , depthFirstGraphSearch, iterativeDeepeningSearch
               , greedyBestFirstSearch, minimumCostSearch, aStarSearch']

allSearcherNames = [ "BreadthFirstTreeSearch", "BreadthFirstGraphSearch"
                   , "DepthFirstGraphSearch", "IterativeDeepeningSearch"
                   , "GreedyBestFirstSearch", "MinimumCostSearch", "A*Search"]
