{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}

module AI.Search.CSP where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map, (!))
import System.Random

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O

import AI.Util.Queue
import AI.Util.Util

-- |Data type used for domains.
type Domain a b = Map a [b]

-- |Data type used for neighbor lists.
type Neighbour a = Map a [a]

-- |Data type used for assignments.
type Assignment a b = Map a b

-- |This class describes finite-domain Constraint Satisfaction Problems.
--  A CSP is specified by the following three inputs:
--
--  * vars        A list of variables; each is atomic (eg Int or String)
--
--  * domains     A map of (var, val) entries
--
--  * neighbours  A map of (var, [var]) that for each variable lists the
--                other variables that participate in the constraints.
--
--  * constraints A function @f A a B b@ that returns @True@ if neighbours A, B
--                satisfy the constraint when @A == a@ and @B == b@.
--
--  In the textbook and in most mathematical definitions, the constraints are
--  specified as explicit pairs of allowable values, but the formulation here
--  is easier to express and more compact in most cases. (For example, the
--  n-Queens problem can be represented in O(n) space using this notation,
--  instead of O(n^4) for the explicit representation.)
--
--  The class also supports data structures and methods that help you
--  solve CSPs by calling a search function on the CSP.
class (Ord v, Eq a) => CSP c v a where

    -- |A list of viables.
    vars :: c v a -> [v]

    -- |A mapping of viables to possible aues.
    domains :: c v a -> Domain v a

    -- |A mapping of viables to a list of the other viables that
    --  participate in its constraints.
    neighbours :: c v a -> Neighbour v

    -- |A function @f A a B b@ that returns True if neighbours A, B satisfy
    --  the constraint when they have aues A == a and B == b.
    constraints :: c v a -> v -> a -> v -> a -> Bool

-------------------
-- CSP Functions --
-------------------

-- |Add (v, a) to a map of current assignments, discarding the old
--  aue if any. Also update the current domain if necessary.
assign :: CSP c v a => c v a -> v -> a -> Backtracking v a ()
assign csp var val = do
    modifyAssignment (M.insert var val)
    whenM useFc  $ forwardCheck csp var val
    whenM useMac $ ac3 csp [ (x,var) | x <- neighbours csp ! var ]

-- |Remove (v, a) from assignments, i.e. backtrack. Do not call this
--  if you are assigning v to a new value - just call 'assign' for that.
--  Also resets the current domain for this variable to the full domain
--  allowed by the CSP.
unassign :: CSP c v a => c v a -> v -> Backtracking v a ()
unassign csp var = do
    modifyAssignment $ M.delete var
    modifyDomain     $ M.insert var (domains csp ! var)

-- |Do forward checking (current domain reduction) for a (var, val) pair.
forwardCheck :: CSP c v a => c v a -> v -> a -> Backtracking v a ()
forwardCheck csp var val = do
    pruned <- getPruned
    assgn  <- getAssignment
    -- Restore prunings from previous value of var.
    forM_ (pruned ! var) $ \(x,y) -> modifyDomain (restore x y)
    -- Remove all prunings.
    putPruned (mkUniversalMap (vars csp) [])
    -- Prune any other assignment that conflicts with var = val.
    forM_ (neighbours csp ! var) $ \x -> when (x `M.notMember` assgn) $
        do dom <- getDomain
           forM_ (dom ! x) $ \y -> when (not $ constraints csp var val x y) $
                do modifyDomain (prune y)
                   modifyPruned (add x y)
    where
        restore x y = M.adjust (y:) x
        prune y     = M.adjust (L.delete y) var
        add x y     = M.adjust ((x,y):) var

-- |Return a list of variables in the current assignment that are in conflict.
conflictedVars :: CSP c v a => c v a -> Assignment v a -> [v]
conflictedVars csp a =
    [ v | v <- vars csp, hasConflicts csp v (a ! v) a ]

-- |Check if an assignment is complete, i.e. there are no more viables
--  left to assign.
allAssigned :: CSP c v a => c v a -> Assignment v a -> Bool
allAssigned csp assignment = M.size assignment == length (vars csp)

hasConflicts :: CSP c v a => c v a -> v -> a -> Assignment v a -> Bool
hasConflicts  csp v a  = not . M.null . M.filterWithKey conflict 
    where
        conflict x y = not $constraints csp v a x y

-- |Return the number of conflicts that v == a has with other
--  viables currently assigned.
numConflicts :: CSP c v a => c v a -> v -> a -> Assignment v a -> Int
numConflicts csp v a assignment = M.foldWithKey count 0 assignment
    where
        ok = constraints csp v a
        count k v n  = if ok k v then n else (n+1)


-- |The goal is to assign all vars with all constraints satisfied.
goalTest :: CSP c v a => c v a -> Assignment v a -> Bool
goalTest csp assignment =
    allAssigned csp assignment && all noConflicts (vars csp)
    where
        noConflicts var = not $
            hasConflicts csp var (assignment ! var) assignment

----------------------------------
-- Backtracking Search for CSPs --
----------------------------------

-- |Backtracking search. This is a wrapper for 'recursiveBacktracking'. You
--  need to supply the problem to be solved, and a set of options. The easiest
--  (but slowest) method is to call this function with 'defaultOpts'.
backtrackingSearch :: CSP c var val =>
                      c var val     -- ^ Constraint Satisfaction Problem
                   -> Opts          -- ^ Search options
                   -> Maybe (Assignment var val)
backtrackingSearch csp opts =
    evalBacktracking (recursiveBacktracking csp) (domains csp) opts

-- |Recursive backtracking search. This is the main workhorse. We make use of
--  the 'Backtracking' monad, which stores the current variable assignments,
--  the current domain and a list of pruned values, as well as a list of
--  options for the search.
recursiveBacktracking :: CSP c v a =>
                         c v a
                      -> Backtracking v a (Maybe (Assignment v a))
recursiveBacktracking csp = getAssignment >>= \assgn ->
    if allAssigned csp assgn
        then return (Just assgn)
        else do
            var  <- selectVariable (vars csp)
            vals <- sortDomainVals csp var
            go var vals
        where
            go var []     = return Nothing
            go var (v:vs) = getAssignment >>= \assgn ->
                if noConflicts var v assgn
                    then do assign csp var v
                            result <- recursiveBacktracking csp
                            if no result
                                then unassign csp var >> go var vs
                                else return result
                    else go var vs
            
            noConflicts var v a = not $ hasConflicts csp var v a 

-- |Select an unassigned variable from the list of variables in a CSP. We may
--  optionally use the Most Constrained Variable heuristic to choose which
--  variable to assign next.
selectVariable :: Ord v => [v] -> Backtracking v a v
selectVariable vs = ifM useMcv (mostConstrained vs) (firstUnassigned vs)

-- |Given a variable in a CSP, select an order in which to try the allowed
--  values for that variable. We may optionally use the Least Constraining
--  Variable heuristic to try values with fewest conflicts first.
sortDomainVals :: CSP c v a => c v a -> v -> Backtracking v a [a]
sortDomainVals csp var = ifM useLcv (leastConstraining csp var) (allValues var)




-- |Return the most constrained variable from a problem. The idea is to speed
--  up the search algorithm by reducing the branching factor.
mostConstrained :: Ord v => [v] -> Backtracking v a v
mostConstrained vs = do
    dom   <- getDomain
    assgn <- getAssignment
    let unassigned = [ v | v <- vs, v `M.notMember` assgn ]
    return $! (argMin unassigned $ numLegalValues dom)
    


-- |Return the first unassigned variable in the problem.
firstUnassigned :: Ord v => [v] -> Backtracking v a v
firstUnassigned (v:vs) = getAssignment >>= select vs
    where
        select vs assgn = if v `M.notMember` assgn
            then return v
            else firstUnassigned vs

-- |Return a list of values for a given variable, sorted in order of least
--  constraining to most constraining.
leastConstraining :: CSP c v a => c v a -> v -> Backtracking v a [a]
leastConstraining csp var = do
    dom   <- getDomain
    assgn <- getAssignment
    return . sortWith (numConf assgn)  $! (dom ! var)
    where
        numConf a val = numConflicts csp var val a
        sortWith :: Ord b => (a -> b) -> [a] -> [a]
        sortWith f = map snd. L.sortBy (O.comparing fst) . map (\x -> (f x, x))

-- |Return a list of all possible values for a given variable, without doing
--  any sorting.
allValues :: Ord v => v -> Backtracking v a [a]
allValues var = getDomain >>= \dom -> return $ dom ! var

-- |Return the number of legal values for a variable.
numLegalValues :: Ord v => Domain v a -> v -> Int
numLegalValues dom var = length (dom ! var)

--------------------------------------
-- Constraint Propagation with AC-3 --
--------------------------------------

-- |The arc-consistency algorithm AC-3 to reduce the domains of a constraint
--  satisfaction problem until they are arc-consistent. There is no return
--  value. This function is only called for its effects on the current domain.
ac3 :: (CSP c v a, Queue q) =>
       c v a                -- ^ CSP
    -> q (v,v)              -- ^ Variables to be tested
    -> Backtracking v a ()  -- ^ Restricted domain or Nothing
ac3 csp queue = when (notEmpty queue) $
    do revised <- removeInconsistentValues csp x y
       if not revised
          then ac3 csp rest
          else do dom <- getDomain
                  when (notNull $ dom ! x) (ac3 csp queue')
    where
        ((x,y), rest) = pop queue
        queue'        = extend new rest
        new           = [ (z,x) | z <- L.delete y (neighbours csp ! x) ]

-- |Remove values for a variable that are inconsistent with the constraints.
--  Returns a 'Bool' flag indicating whether the domain has been revised or not.
--  An /inconsistent/ value for @x@ is one for which we can't find any value in
--  the domain of @y@ such that the constraints are satisfied.
removeInconsistentValues :: CSP c v a =>
                            c v a   -- ^ CSP
                         -> v       -- ^ Var to restrict
                         -> v       -- ^ Var to check against
                         -> Backtracking v a Bool
removeInconsistentValues csp x y = getDomain >>= \dom -> do

    let old = dom ! x
        new = filter fun old
        fun xv = any (\yv -> constraints csp x xv y yv) (dom ! y)

    if length new < length old
        then (modifyDomain $ M.insert x new) >> return True
        else return False

---------------------------------------
-- Min-Conflicts Hillclimbing Search --
---------------------------------------

-- |Solve a Constraint Satisfaction Problem by stochastic hillclimbing on the
--  number of conflicts.
minConflictsIO :: CSP c v a => c v a -> Int -> IO (Assignment v a)
minConflictsIO csp maxSteps = do
    g <- getStdGen
    return (minConflicts g csp maxSteps)

-- |Solve a Constraint Satisfaction Problem by stochastic hillclimbing on the
--  number of conflicts. This is the /pure/ version of the algorithm. You must
--  supply a random number generator. See also 'minConflictsIO'.
minConflicts :: (CSP c v a, RandomGen g) =>
                g
             -> c v a
             -> Int
             -> Assignment v a
minConflicts gen csp maxSteps = go g 0 initial
    where
        (initial, g) = initialAssignment gen csp

        go g steps current =
            let conflicted = conflictedVars csp current
             in if steps == maxSteps || null conflicted
                    then current 
                    else let (var, g1) = randomChoice g conflicted
                             (val, g2) = minConflictsValue g1 csp var current
                          in go g2 (steps+1) (M.insert var val current)

-- |The initial assignment for the min-conflicts algorithm. We choose the
--  assignments according to the minimum-conflicts heuristic, breaking ties
--  at random.
initialAssignment :: (CSP c v a, RandomGen g) => g -> c v a -> (Assignment v a, g)
initialAssignment g csp = go g (vars csp) M.empty
    where
        go g []         current = (current, g)
        go g (var:rest) current = 
            let (val, g') = minConflictsValue g csp var current
             in go g' rest (M.insert var val current)
            

-- |Return the value that will give a variable the least number of conflicts.
--  If there is a tie, choose at random.
minConflictsValue :: (CSP c v a, RandomGen g) => g -> c v a -> v -> Assignment v a -> (a, g)
minConflictsValue g csp var current =
    argMinRandom g (domains csp ! var) (\v -> numConflicts csp var v current)

-----------------------
-- Utility Functions --
-----------------------

-- |Options for recursive backtracking. We allow the following options:
--  
--  * 'mcv' - use the Most Constrained Variable heuristic
--
--  * 'lcv' - use the Least Constraining Variable heuristic
--
--  * 'fc'  - use Forward Checking
--
--  * 'mac' - use Maintaining Arc Consistency
data Opts = Opts
    { mcv :: Bool
    , lcv :: Bool
    , fc  :: Bool
    , mac :: Bool }

-- |Default options for backtracking search. Doesn't use any heuristics.
defaultOpts :: Opts
defaultOpts = Opts False False False False

-- |Use 'optimized' options for backtracking search. We maintain arc consistency
--  at each stage, and use the most constrained variable and least constraining
--  variable heuristics.
fastOpts :: Opts
fastOpts = Opts True True False True

-- |Use Most Constrained Variable heuristic?
useMcv :: MonadReader Opts m => m Bool
useMcv = ask >>= return . mcv

-- |Use Least Constraining Variable heuristic?
useLcv :: MonadReader Opts m => m Bool
useLcv = ask >>= return . lcv

-- |Use Forwarc Checking?
useFc  :: MonadReader Opts m => m Bool
useFc = ask >>= return . fc

-- |Use Maintaining Arc Consistency?
useMac :: MonadReader Opts m => m Bool
useMac = ask >>= return . mac

-- |State variables for search in the 'Backtracking' monad.
type BTState a b = (Domain a b, Map a [(a,b)], Assignment a b)

-- |Monad for backtracking search. We use a @StateT@ monad to keep track of the
--  current domain, pruned values and assigned values, and wrap a @Reader Opts@
--  monad to keep track of the various search options.
type Backtracking a b c = StateT (BTState a b) (Reader Opts) c

-- |Use this to run a computation in the 'Backtracking' monad to extract
--  the final state and value.
runBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> (c, BTState a b)
runBacktracking computation dom opts = 
    runReader (runStateT computation (dom, prune, assgn)) opts
    where
        prune = mkUniversalMap (M.keys dom) []
        assgn = M.empty

-- |Use this to evaluate the result of computation in the 'Backtracking' monad.
evalBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> c
evalBacktracking c dom opts = fst $ runBacktracking c dom opts

-- |Use this to evaluate the final state of a computation in the 'Backtracking'
--  monad.
execBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> BTState a b
execBacktracking c dom opts = snd $ runBacktracking c dom opts

-- |Return the current (constrained) domains in backtracking search.
getDomain :: MonadState (a,b,c) m => m a
getDomain = get >>= return . fst3

-- |Modify the current constrained domains in backtracking search.
modifyDomain :: MonadState (a,b,c) m => (a -> a) -> m ()
modifyDomain f = modify $ \(x,y,z) -> (f x,y,z)

-- |Return the list of pruned variables in backtracking search.
getPruned :: MonadState (a,b,c) m => m b
getPruned = get >>= return . snd3

-- |Store a new set of pruned values
putPruned :: MonadState (a,b,c) m => b -> m ()
putPruned p = get >>= \(x,y,z) -> put (x,p,z)

-- |Modify the list of pruned variables in backtracking search.
modifyPruned :: MonadState (a,b,c) m => (b -> b) -> m ()
modifyPruned f = modify $ \(x,y,z) -> (x,f y,z)

-- |Return the current assignment list in backtracking search.
getAssignment :: MonadState (a,b,c) m => m c
getAssignment = get >>= return . thd3

-- |Store a new assignment in place of the old one.
putAssignment :: MonadState (a,b,c) m => c -> m ()
putAssignment a = get >>= \(x,y,z) -> put (x,y,a)

-- |Modify the list of assignments in backtracking search.
modifyAssignment :: MonadState (a,b,c) m => (c -> c) -> m ()
modifyAssignment f = modify $ \(x,y,z) -> (x,y,f z)

