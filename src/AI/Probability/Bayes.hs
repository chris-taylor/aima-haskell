module AI.Probability.Bayes
    ( BayesNet
    , fromList
    , enumerationAsk
    , eliminationAsk
    , rejectionAsk
    , likelihoodWeighting ) where

import AI.Util.ProbDist
import AI.Util.Array
import AI.Util.Util

import Control.DeepSeq
import Control.Monad
import Data.Map (Map, (!))
import Data.Ord (comparing)
import qualified Control.Monad.Random as R
import qualified Data.List as L
import qualified Data.Map as M

---------------
-- Bayes Net --
---------------

-- |A node in a Bayes Net. We keep things very lightweight, storing just a
--  list of the node's parents and its conditional probability table as a list.
data Node e = Node { nodeParents :: [e]
                   , nodeCond :: [Prob] } deriving (Show)

-- |A Bayes Net contains two fields - a list of variables ordered from parents
--  to children, and a 'Map' from variable names to 'Node's.
data BayesNet e = BayesNet { bnVars :: [e]
                           , bnMap :: Map e (Node e) } deriving (Show)

-- |This function creates a Bayes Net from a list of elements of the form
--  (variable, parents, conditional probs). The conditional probability table
--  is specified with the first parent varying most slowly. For exampleif the
--  parents are A and B, and the conditional probability table is
--
--  > A | B | Prob
--  > --+---+-----
--  > T | T | 0.9
--  > T | F | 0.8
--  > F | T | 0.7
--  > F | F | 0.1
--
--  then the list of probabilities should be @[0.9,0.8,0.7,0.1]@.
fromList :: Ord e => [ (e, [e], [Prob]) ] -> BayesNet e
fromList xs = BayesNet vars net
    where
        vars = L.sortBy (comparing rank) (M.keys net)

        net  = foldr go M.empty xs

        go (ev,ps,cond) = if length cond /= 2 ^ length ps
            then error "Invalid length for probability table"
            else M.insert ev (Node ps cond)

        rank e = if null ps then 0 else 1 + maximum (map rank ps)
            where ps = nodeParents (net ! e)

---------------------
-- Enumeration Ask --
---------------------

-- |The Enumeration-Ask algorithm. This iterates over variables in the Bayes
--  Net, from parents to children, summing over the possible values when a
--  variable is not assigned. It uses the helper function 'enumerateAll'.
enumerationAsk :: Ord e => BayesNet e -> [(e,Bool)] -> e -> Dist Bool
enumerationAsk bn fixed e = normalize $ D [(True, p True), (False, p False)]
    where
        p x = enumerateAll bn (M.insert e x a) (bnVars bn)
        a   = M.fromList fixed

-- |A helper function for 'enumerationAsk'. This performs the hard work of
--  enumerating all unassigned values in the network and summing over their
--  conditional probabilities.
enumerateAll :: Ord e => BayesNet e -> Map e Bool -> [e] -> Prob
enumerateAll bn a []     = 1.0
enumerateAll bn a (v:vs) = if v `M.member` a
    then bnProb bn a (v, a!v) * enumerateAll bn a vs
    else p * go True + (1 - p) * go False
    where
        p    = bnProb bn a (v,True)
        go x = enumerateAll bn (M.insert v x a) vs

---------------------
-- Elimination Ask --
---------------------

-- |A factor in the variable elimination algorithm. A factor is a list of
--  variables (unfixed by the problem) and a conditional probability table
--  associated with them.
data Factor e = Factor { fVars :: [e], fVals :: [Prob] } deriving (Show)

-- |Exact inference using the elimination-ask algorithm.
eliminationAsk :: Ord e => BayesNet e -> [(e,Bool)] -> e -> Dist Bool
eliminationAsk bn fixed e = go [] (reverse $ bnVars bn)
    where
        go factors []     = let f = pointwiseProduct factors
                            in normalize $ D $ zip [True,False] (fVals f)

        go factors (v:vs) = let factors' = (mkFactor bn fixed v) : factors
                            in if v `elem` hidden
                                then go [sumOut v factors'] vs
                                else go factors' vs

        hidden = (e:map fst fixed) `deleteAll` bnVars bn

-- |Given a Bayes Net, a list of fixed variables and a target variable, return
--  a factor to be used in the 'eliminationAsk' algorithm.
mkFactor :: Ord e => BayesNet e -> [(e,Bool)] -> e -> Factor e
mkFactor bn fixed e = Factor fvar (subSlice cond is)
    where   
        vars = e : bnParents bn e
        cond = bnCond bn e ++ map (1-) (bnCond bn e)
        fvar = map fst fixed `deleteAll` vars
        is   = getIxVector vars fixed

-- |Return the pointwise product of a list of factors. This is simply a strict
--  fold over the factors using 'mulF'.
pointwiseProduct :: Eq e => [Factor e] -> Factor e
pointwiseProduct = L.foldl1' mulF

-- |Sum out a factor with respect to one of its variables.
sumOut :: Eq e => e -> [Factor e] -> Factor e
sumOut e factors = with True `addF` with False
    where
        with x = pointwiseProduct $ map (set e x) factors

-- |Return the pointwise product of two factors. This is ugly at the moment!
--  It should be refactored to (a) be prettier and (b) use an 'Array' instead
--  of a list to store the factor values, as the huge amount of list indexing
--  going on will probably be really inefficient.
mulF :: Eq e => Factor e -> Factor e -> Factor e
mulF f1 f2 = Factor vars (map f vals)
    where
        vars   = L.union (fVars f1) (fVars f2)
        vals   = bools (length vars)

        f bs = valueAt bs (getIxs f1) (fVals f1) * valueAt bs (getIxs f2) (fVals f2)

        getIxs factor     = map (vars `indexOf`) (fVars factor)
        valueAt bs ns vals = vals !! bnSubRef (bs `elemsAt` ns)

-- |Return the pointwise sum of two factors. This performs a quick sanity check,
--  requiring that the factors have the same variables in the same order.
addF :: Eq e => Factor e -> Factor e -> Factor e
addF (Factor vs1 ps1) (Factor vs2 ps2) = if vs1 /= vs2
    then error "Can't add factors with different variables"
    else Factor vs1 $ zipWith (+) ps1 ps2

-- |Take a slice of a factor by setting one of its variables to a fixed value.
--  This is a helper function for 'sumOut'.
set :: Eq e => e -> Bool -> Factor e -> Factor e
set e x (Factor vs ps) = if not (e `elem` vs)
                            then Factor vs ps
                            else Factor (L.delete e vs) (subSlice1 ps (i,x))
                            where
                                i = vs `indexOf` e

------------------------
-- Rejection Sampling --
------------------------

-- |Random sample from a Bayes Net.
bnSample :: Ord e => BayesNet e -> IO (Map e Bool)
bnSample bn = go M.empty (bnVars bn)
    where
        go assignment []     = return assignment
        go assignment (v:vs) = do
            let p = bnProb bn assignment (v,True)
            x <- probabilityIO p
            go (M.insert v x assignment) vs

-- |Rejection sampling algorithm.
rejectionAsk :: Ord e => Int -> BayesNet e -> [(e,Bool)] -> e -> IO (Dist Bool)
rejectionAsk nIter bn fixed e =
    foldM func initial [1..nIter] >>= return . weighted . M.toList

    where
        func m _ = do
            a <- bnSample bn
            if isConsistent a
                then let x = a!e
                     in return $! x `seq` M.adjust (+1) x m
                else return m

        initial = M.fromList [(True,0),(False,0)]

        isConsistent a = map (a!) vars == vals

        (vars,vals) = unzip fixed

--------------------------
-- Likelihood Weighting --
--------------------------

-- |Random sample from a Bayes Net, with an associated likelihood weight. The
--  weight gives the likelihood of the fixed evidence, given the sample.
weightedSample :: Ord e => BayesNet e -> [(e,Bool)] -> IO (Map e Bool, Prob)
weightedSample bn fixed =
    go 1.0 (M.fromList fixed) (bnVars bn)
    where
        go w assignment []     = return (assignment, w)
        go w assignment (v:vs) = if v `elem` vars
            then
                let w' = w * bnProb bn assignment (v, fixed %! v)
                in go w' assignment vs
            else do
                let p = bnProb bn assignment (v,True)
                x <- probabilityIO p
                go w (M.insert v x assignment) vs

        vars = map fst fixed

-- |Repeatedly draw likelihood-weighted samples from a distribution to infer
--  probabilities from a Bayes Net.
likelihoodWeighting :: Ord e => Int -> BayesNet e -> [(e,Bool)] -> e -> IO (Dist Bool)
likelihoodWeighting nIter bn fixed e = 
    foldM func initial [1..nIter] >>= distribution

    where
        func m _ = do
            (a, w) <- weightedSample bn fixed
            let x = a ! e
            return $! x `seq` w `seq` M.adjust (+w) x m

        initial = M.fromList [(True,0),(False,0)]

        distribution = return . normalize . D . M.toList

-------------------------
-- Bayes Net Utilities --
-------------------------

-- |Given a set of assignments and a variable, this function returns the values
--  of the variable's parents in the assignment, in the order that they are
--  specified in the Bayes Net.
bnVals :: Ord e => BayesNet e -> Map e Bool -> e -> [Bool]
bnVals bn a x = map (a!) (bnParents bn x)

-- |Return the parents of a specified variable in a Bayes Net.
bnParents :: Ord e => BayesNet e -> e -> [e]
bnParents (BayesNet _ m) x = nodeParents (m ! x)

-- |Return the conditional probability table of a variable in a Bayes Net.
bnCond :: Ord e => BayesNet e -> e -> [Prob]
bnCond (BayesNet _ m) x = nodeCond (m ! x)

-- |Given a set of assignments and a (variable,value) pair, this function
--  returns the probability that the variable has that value, given the
--  assignments. Note that the variable's parents must be already assigned
--  (this is why it is important to perform the enumeration of variables from
--  parents to children).
bnProb :: Ord e => BayesNet e -> Map e Bool -> (e, Bool) -> Prob
bnProb bn a (v,b) = if b then p else 1 - p
    where p = bnCond bn v !! bnSubRef (bnVals bn a v)

-- |A helper function for 'bnProb'. Given a list of parent values, this returns
--  the correct index for a probability to be extracted from the conditional
--  probability table associated with a variable.
bnSubRef :: [Bool] -> Int
bnSubRef = ndSubRef . map (\x -> if x then 0 else 1)
