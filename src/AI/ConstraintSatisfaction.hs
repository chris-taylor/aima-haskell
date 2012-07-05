{-# LANGUAGE MultiParamTypeClasses #-}

module AI.ConstraintSatisfaction where

import Data.Map (Map, (!))

import qualified Data.Map as M

import AI.Util.Util

-- |This class describes finite-domain Constraint Satisfaction Problems.
--	A CSP is specified by the following three inputs:
--  	vars 		A list of variables; each is atomic (eg Int or String)
-- 		domains 	A map of (var, val) entries
-- 		neighbours 	A map of (var, [var]) that for each variable lists the
--					other variables that participate in the constraints.
--		constraints A function @f A a B b@ that returns @True@ if neighbours
-- 					A, B satisfy the constraint when @A == a@ and @B == b@.
--	In the textbook and in most mathematical definitions, the constraints are
--	specified as explicit pairs of allowable values, but the formulation here
--	is easier to express and more compact in most cases. (For example, the
--  n-Queens problem can be represented in O(n) space using this notation,
--	instead of O(n^4) for the explicit representation.) In terms of describing
--	the CSP as a problem, that's all there is.
--
--	However, the class also supports data structures and methods that help you
--	solve CSPs by calling a search function on the CSP. Methods and slots are
--	as follows, where the argument @a@ represents an assignment, which is a
--  map of (var, val) entries.
class Ord var => CSP c var val where

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
	nConflicts csp var val assignment = countIf conflict assignedVals
		where
			assignedVals   = M.toList assignment
			conflict (x,y) = not (constraints csp var val x y)

	-- | Do forward checking (current domain reduction) for this assignment.
	forwardCheck :: c var val -> var -> val -> Map var val -> ()
	forwardCheck = undefined

	-- Return a list of (action, state) pairs.
	-- succ :: c var val -> 

	-- | The goal is to assign all vars with all constraints satisfied.
	goalTest :: c var val -> Map var val -> Bool
	goalTest csp assignment =
		M.size assignment == length (vars csp) && all noConflicts (vars csp)
		where
			noConflicts v = 
				nConflicts csp v (assignment ! v) assignment == 0



