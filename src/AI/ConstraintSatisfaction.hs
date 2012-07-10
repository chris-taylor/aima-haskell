{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module AI.ConstraintSatisfaction where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map, (!))

import qualified Data.List as L
import qualified Data.Map as M

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
class Ord v => CSP c v a where

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
    nConflicts :: c v a -> v -> a -> Assignment v a -> Int
    nConflicts csp v a assignment = countIf conflict assignedVals
        where
            assignedVals   = M.toList assignment
            conflict (x,y) = not (constraints csp v a x y)

    -- |Add (v, a) to a map of current assignments, discarding the old
    --  aue if any. Also update the current domain if necessary.
    assign :: Ord v =>
              c v a
           -> v
           -> a
           -> Assignment v a
           -> Backtracking v a (Assignment v a)
    assign csp var val assignment = return (M.insert var val assignment)

    -- |Remove (v, a) from assignments, i.e. backtrack. Do not call this
    --  if you are assigning v to a new aue - just call 'assign' for that.
    --  Also resets the current domain for this viable to the full domain
    --  allowed by the CSP.
    unassign :: Ord v =>
                c v a
             -> v
             -> Assignment v a
             -> Backtracking v a (Assignment v a)
    unassign csp var assignment = do
        modifyDomain $ M.insert var (domains csp ! var)
        return (M.delete var assignment)

    -- |Do forward checking (current domain reduction) for this assignment.
    forwardCheck :: c v a -> v -> a -> Map v a -> ()
    forwardCheck = undefined

    -- Return a list of (action, state) pairs.
    -- succ :: c v a -> 

    -- |The goal is to assign all vars with all constraints satisfied.
    goalTest :: c v a -> Assignment v a -> Bool
    goalTest csp assignment =
        allAssigned csp assignment && all noConflicts (vars csp)
        where
            noConflicts var = 
                nConflicts csp var (assignment ! var) assignment == 0

    -- |Check if an assignment is complete, i.e. there are no more viables
    --  left to assign.
    allAssigned :: c v a -> Assignment v a -> Bool
    allAssigned csp assignment = M.size assignment == length (vars csp)



-- |Monad for backtracking search. We use a @StateT@ monad to keep track of the
--  current domain and pruned values, and wrap a @Reader Opts@ monad to keep
--  track of the various search options.
type Backtracking a b c = StateT (Domain a b, Domain a b) (Reader Opts) c

runBacktracking :: Backtracking a b c -> Domain a b -> Opts -> c
runBacktracking computation dom opts =
    runReader (evalStateT computation (dom, M.empty)) opts

getDomain :: MonadState (a,b) m => m a
getDomain = get >>= return . fst

getPruned :: MonadState (a,b) m => m b
getPruned = get >>= return . snd

modifyDomain :: MonadState (a,b) m => (a -> a) -> m ()
modifyDomain f = modify $ \(x,y) -> (f x, y)

modifyPruned :: MonadState (a,b) m => (b -> b) -> m ()
modifyPruned f = modify $ \(x,y) -> (x, f y)


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
        
---------------------
-- Search for CSPs --
---------------------

-- |Options for recursive backtracking. We allow the following options:
--  
--  * 'useMCV' - use the Most Constrained Variable heuristic
--
--  * 'useLCV' - use the Least Constraining Variable heuristic
--
--  * 'useFC'  - use Forward Checking
--
--  * 'useMAC' - use Maintaining Arc Consistency
data Opts = Opts
    { mcv :: Bool
    , lcv :: Bool
    , fc  :: Bool
    , mac :: Bool }

defaultOpts = Opts False False False False

useMcv :: MonadReader Opts m => m Bool
useMcv = ask >>= return . mcv

useLcv :: MonadReader Opts m => m Bool
useLcv = ask >>= return . lcv

useFc  :: MonadReader Opts m => m Bool
useFc = ask >>= return . fc

useMac :: MonadReader Opts m => m Bool
useMac = ask >>= return . mac

-- |Backtracking search. This is a wrapper for `recursiveBacktracking' which
--  sets up the options and the initial domain.
backtrackingSearch :: CSP c var val => c var val -> Maybe (Assignment var val)
backtrackingSearch csp =
    runBacktracking (recursiveBacktracking csp M.empty) (domains csp) defaultOpts

-- |Recursive backtracking search. This is the main workhorse. We make use of
--  the state monad to store the current domains for each variable.
recursiveBacktracking :: CSP c v a =>
                         c v a
                      -> Assignment v a
                      -> Backtracking v a (Maybe (Assignment v a))
recursiveBacktracking csp assignment = if allAssigned csp assignment
    then return (Just assignment)
    else do
        var  <- selectUnassignedVariable csp assignment
        vals <- orderDomainValues var assignment
        go assignment var vals
    where
        go assignment var []     = return Nothing
        go assignment var (v:vs) = if nConflicts csp var v assignment == 0
            then do
                assignment' <- assign csp var v assignment
                result      <- recursiveBacktracking csp assignment'
                if no result
                    then do
                        assignment' <- unassign csp var assignment
                        go assignment' var vs
                    else return result
            else go assignment var vs

-- |Select an unassigned variable from the list of variables in a CSP. We may
--  optionally use the heuristics Most Constrained Variable heuristic to choose
--  which variable to assign next.
selectUnassignedVariable :: CSP c var val => c var val -> Assignment var val -> Backtracking var val var
selectUnassignedVariable csp assignment = return $ selectFrom (vars csp)
    where
        selectFrom []     = error "No unassigned variables -- SELECTUNASSIGNEDVARIABLE"
        selectFrom (v:vs) = if v `M.notMember` assignment
            then v
            else selectFrom vs

-- |Given a variable in a CSP, select an order in which to try the allowed
--  values for that variable. We may optionally use the Least Constraining
--  Variable heuristic to try values with fewest conflicts first.
orderDomainValues :: (Ord v) => v -> Assignment v a -> Backtracking v a [a]
orderDomainValues var assignment = do
    dom <- getDomain
    return (dom ! var)

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