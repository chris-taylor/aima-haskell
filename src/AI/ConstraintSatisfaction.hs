{-# LANGUAGE MultiParamTypeClasses #-}

module AI.ConstraintSatisfaction where

import Data.Map (Map)

import qualified Data.Map as M

-- |Class for constraint satisfaction problems with variable type 'var' and
--  values of type 'val'.
class CSP c var val where

	-- | A list of variables.
	vars :: c var val -> [var]

	-- | A mapping of variables to possible values.
	domains :: c var val -> Map var [val]

	-- | A mapping of variables to a list of the other variables that
	--   participate in its constraints.
	neighbours :: c var val -> Map var [var]

	-- | A function @f A a B b@ that returns True if neighbours A, B satisfy
	--   the constraint when they have values A == a and B == b.
	constraints :: c var val -> var -> val -> var -> val -> Bool

	-- | Add (var, val) to a map of current assignments, discarding the old
	--   value if any.
	assign :: var -> val -> Map var val -> Map var val
	assign = M.insert

	-- | Remove (var, val) from assignments, i.e. backtrack. Do not call this
	--   if you are assigning var to a new value - just call 'assign' for that.
	unassign :: var -> Map var val -> Map var val
	unassign = M.delete

	-- | Return the number of conflicts that var == val has with other
	--   variables currently assigned.
	nConflicts :: c var val -> var -> val -> Map var val -> Int
	nConflicts csp var val assgn = countIf conflict assignedVals
		where
			assignedVals   = M.toList assgn
			conflict (x,y) = constraints csp var val x y

	-- | Do forward checking (current domain reduction) for this assignment.
	forwardCheck :: c var val -> var -> val -> Map var val -> ()
	forwardCheck = undefined

