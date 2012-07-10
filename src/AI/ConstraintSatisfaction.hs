{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

module AI.ConstraintSatisfaction where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map, (!))

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

    -- |Return the number of conflicts that v == a has with other
    --  viables currently assigned.
    numConflicts :: c v a -> v -> a -> Assignment v a -> Int
    numConflicts csp v a assignment = countIf conflict assignedVals
        where
            assignedVals   = M.toList assignment
            conflict (x,y) = not (constraints csp v a x y)

    -- |The goal is to assign all vars with all constraints satisfied.
    goalTest :: c v a -> Assignment v a -> Bool
    goalTest csp assignment =
        allAssigned csp assignment && all noConflicts (vars csp)
        where
            noConflicts var = 
                numConflicts csp var (assignment ! var) assignment == 0

    -- |Check if an assignment is complete, i.e. there are no more viables
    --  left to assign.
    allAssigned :: c v a -> Assignment v a -> Bool
    allAssigned csp assignment = M.size assignment == length (vars csp)


-- |Add (v, a) to a map of current assignments, discarding the old
--  aue if any. Also update the current domain if necessary.
assign :: CSP c v a => c v a -> v -> a -> Backtracking v a ()
assign csp var val = do
    modifyAssignment (M.insert var val)
    whenM useFc  $ forwardCheck csp var val
    whenM useMac $ (ac3 csp [ (x,var) | x <- neighbours csp ! var ]) >> return ()

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
    runBacktracking (recursiveBacktracking csp) (domains csp) opts

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
            var  <- selectUnassignedVariable (vars csp)
            vals <- orderDomainValues csp var
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
            
            noConflicts var v a = numConflicts csp var v a == 0

-- |Select an unassigned variable from the list of variables in a CSP. We may
--  optionally use the Most Constrained Variable heuristic to choose which
--  variable to assign next.
selectUnassignedVariable :: Ord v => [v] -> Backtracking v a v
selectUnassignedVariable vs = ifM useMcv
    (mostConstrained vs)
    (firstUnassigned vs)

-- |Return the most constrained variable from a problem. The idea is to speed
--  up the search algorithm by reducing the branching factor.
mostConstrained :: Ord v => [v] -> Backtracking v a v
mostConstrained vs = do
    dom   <- getDomain
    assgn <- getAssignment
    let unassigned = [ v | v <- vs, v `M.notMember` assgn ]
    return (argMin unassigned $ negate . numLegalValues dom)

-- |Return the first unassigned variable in the problem.
firstUnassigned :: Ord v => [v] -> Backtracking v a v
firstUnassigned (v:vs) = getAssignment >>= \assgn ->
    if v `M.notMember` assgn
        then return v
        else selectUnassignedVariable vs

-- |Given a variable in a CSP, select an order in which to try the allowed
--  values for that variable. We may optionally use the Least Constraining
--  Variable heuristic to try values with fewest conflicts first.
orderDomainValues :: CSP c v a => c v a -> v -> Backtracking v a [a]
orderDomainValues csp var = ifM useLcv
    (leastConstraining csp var)
    (allValues var)

-- |Return a list of values for a given variable, sorted in order of least
--  constraining to most constraining.
leastConstraining :: CSP c v a => c v a -> v -> Backtracking v a [a]
leastConstraining csp var = do
    dom   <- getDomain
    assgn <- getAssignment
    return $ L.sortBy (O.comparing $ numConf assgn) (dom ! var)
    where
        numConf a val = numConflicts csp var val a

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
--  satisfaction problem until they are arc-consistent. A @Bool@ flag is also
--  returned, with the value @False@ if an inconsistency is found and @True@ 
--  otherwise.
runAC3 :: CSP c v a => c v a -> Maybe (Domain v a)
runAC3 csp = runBacktracking (ac3 csp initial) (domains csp) defaultOpts
    where
        initial = [ (x, y) | x <- vars csp, y <- neighbours csp ! x ]

-- |The main recursive function, which keeps track of the current
--  works queue and the restricted domains.
ac3 :: (CSP c v a, Queue q) =>
       c v a                                 -- ^ CSP
    -> q (v,v)                               -- ^ Variables to be tested
    -> Backtracking v a (Maybe (Domain v a)) -- ^ Restricted domain or Nothing
ac3 csp queue = if empty queue
    then Just `fmap` getDomain
    else do
        revised <- removeInconsistentValues csp x y
        if not revised
            then ac3 csp rest
            else getDomain >>= \dom -> if null (dom ! x)
                                           then return Nothing
                                           else ac3 csp queue'
    where
        ((x,y), rest) = pop queue
        queue'        = extend new rest
        new           = [ (z,x) | z <- L.delete y (neighbours csp ! x) ]

-- |Returns a new domain for x, together with a Bool flag indicating
--  whether the domain has been revised or not. An /inconsistent/ value for @x@
--  is one for which we can't find any value in the domain of @y@ such that the
--  constraints are satisfied.
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

-- |Default options for backtracking search. Don't use any heuristics.
defaultOpts = Opts False False False False

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

-- |Monad for backtracking search. We use a @StateT@ monad to keep track of the
--  current domain, pruned values and assigned values, and wrap a @Reader Opts@
--  monad to keep track of the various search options.
type Backtracking a b c =
    StateT (Domain a b, Map a [(a,b)], Assignment a b) (Reader Opts) c

-- |Use this to evaluate a computation in the 'Backtracking' monad.
runBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> c
runBacktracking computation dom opts =
    runReader (evalStateT computation (dom, prune, assgn)) opts
    where
        prune = mkUniversalMap (M.keys dom) []
        assgn = M.empty

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

-- |Modify the list of assignments in backtracking search.
modifyAssignment :: MonadState (a,b,c) m => (c -> c) -> m ()
modifyAssignment f = modify $ \(x,y,z) -> (x,y,f z)

-----------------
-- Example CSP --
-----------------

data ExampleCSP a b = ExampleCSP

exampleCSP :: ExampleCSP Char Int
exampleCSP = ExampleCSP

instance CSP ExampleCSP Char Int where
    vars _ = "XY"
    domains _ = M.fromList [ ('X', [1,2]), ('Y', [1]) ]
    neighbours _ = M.fromList [ ('X',"Y"), ('Y',"X") ]
    constraints _ x xv y yv = xv /= yv

----------------------
-- Map Coloring CSP --
----------------------

data MapColoringCSP v a = MC
    { neighboursMC :: Map String [String]
    , colorsMC :: [Char] }

mapColoringCSP :: [(String,[String])] -> [Char] -> MapColoringCSP String Char
mapColoringCSP nbrs colors = MC (M.fromList nbrs) colors

instance CSP MapColoringCSP String Char where
    vars (MC nbrs _) = M.keys nbrs

    domains csp = mkUniversalMap (vars csp) (colorsMC csp)

    neighbours (MC nbrs _) = nbrs

    constraints csp x xv y yv = if y `elem` neighbours csp ! x
        then xv /= yv
        else True

australia :: MapColoringCSP String Char
australia = mapColoringCSP territories "RGB"
    where
        territories = [ ("SA",  ["WA","NT","Q","NSW","V"])
                      , ("NT",  ["WA","Q","SA"])
                      , ("NSW", ["Q","V","SA"])
                      , ("T",   [])
                      , ("WA",  ["SA","NT"])
                      , ("Q",   ["SA","NT","NSW"])
                      , ("V",   ["SA","NSW"]) ]

