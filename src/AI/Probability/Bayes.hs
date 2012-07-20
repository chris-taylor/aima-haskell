module AI.Probability.Bayes where

import AI.Util.ProbDist
import AI.Util.Util

import Data.Map (Map, (!))
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Map as M

---------------
-- Bayes Net --
---------------

-- |A node in a Bayes Net. We keep things very lightweight, storing just a
--  list of the node's parents and its conditional probability table as a list.
data Node e = Node { parents :: [e], cond :: [Prob] } deriving (Show)

-- |A Bayes Net is a 'Map' from variables to the nodes associated with that
--  variable.
newtype BayesNet e = BayesNet (Map e (Node e)) deriving (Show)

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
fromList = BayesNet . foldr go M.empty
    where
        go (ev,cond,ps) = if length ps /= 2 ^ length cond
            then error "Invalid length for probability table"
            else M.insert ev (Node cond ps)

-------------
-- Queries --
-------------

-- |The Enumeration-Ask algorithm. This iterates over variables in the Bayes
--  Net, from parents to children, summing over the possible values when a
--  variable is not assigned. It uses the helper function 'enumerateAll'.
enumerationAsk :: Ord e => BayesNet e -> [(e,Bool)] -> e -> Dist Bool
enumerationAsk bn fixed e = normalize $ D [(True,p),(False,q)]
    where
        p = enumerateAll bn (M.insert e True a) (bnVars bn)
        q = enumerateAll bn (M.insert e False a) (bnVars bn)

        a = M.fromList fixed

-- |A helper function for 'enumerationAsk'. This performs the hard work of
--  enumerating all unassigned values in the network and summing over their
--  conditional probabilities.
enumerateAll :: Ord e => BayesNet e -> Map e Bool -> [e] -> Prob
enumerateAll bn a []     = 1.0
enumerateAll bn a (v:vs) = if v `M.member` a
    then bnProb bn a (v, a!v) * enumerateAll bn a vs
    else let p = bnProb bn a (v,True)
             q = enumerateAll bn (M.insert v True a)  vs
             r = enumerateAll bn (M.insert v False a) vs
         in p * q + (1 - p) * r

-------------------------
-- Bayes Net Utilities --
-------------------------

-- |Enumerate the variables in a Bayes Net, from parents to children.
bnVars :: Ord e => BayesNet e -> [e]
bnVars bn@(BayesNet m) = L.sortBy (comparing $ bnRank bn) (M.keys m)

-- |Given the /rank/ of a variable in a Bayes Net, so that the variables can be
--  ordered. The rank of a variable with no parents is 0. Otherwise, the rank
--  is one more than the maximum of the ranks of the variables parents.
bnRank :: Ord e => BayesNet e -> e -> Int
bnRank bn e = if null ps then 0 else 1 + (maximum $ map (bnRank bn) ps)
    where ps = bnParents bn e

-- |Given a set of assignments and a variable, this function returns the values
--  of the variable's parents in the assignment, in the order that they are
--  specified in the Bayes Net.
bnVals :: Ord e => BayesNet e -> Map e Bool -> e -> [Bool]
bnVals bn a x = map (a!) (bnParents bn x)

-- |Return the parents of a specified variable in a Bayes Net.
bnParents :: Ord e => BayesNet e -> e -> [e]
bnParents (BayesNet m) x = parents (m ! x)

-- |Return the conditional probability table of a variable in a Bayes Net.
bnCond :: Ord e => BayesNet e -> e -> [Prob]
bnCond (BayesNet m) x = cond (m ! x)

-- |Given a set of assignments and a (variable,value) pair, this function
--  returns the probability that the variable has that value, given the
--  assignments. Note that the variable's parents must be already assigned
--  (this is why it is important to perform the enumeration of variables from
--  parents to children).
bnProb :: Ord e => BayesNet e -> Map e Bool -> (e, Bool) -> Prob
bnProb bn a (v,b) = if b then p else 1 - p
    where p = bnCond bn v !! bnIndex (bnVals bn a v)

-- |A helper function for 'bnProb'. Given a list of parent values, this returns
--  the correct index for a probability to be extracted from the conditional
--  probability table associated with a variable.
bnIndex :: [Bool] -> Int
bnIndex bs = sum $ zipWith (*) (reverse $ map toInt bs) (map (2^) [0..])
    where toInt b = if b then 0 else 1

--------------
-- Examples --
--------------

-- |A Bayes Network describing the "is the grass wet?" problem.
grass :: BayesNet Char
grass = fromList [ ('R', "",   [0.2])
                 , ('S', "R",  [0.01, 0.4])
                 , ('G', "SR", [0.99, 0.9, 0.8, 0]) ]

-- |The "alarm" example.
alarm :: BayesNet String
alarm = fromList [ ("Burglary", [], [0.001])
                 , ("Earthquake", [], [0.002])
                 , ("Alarm", ["Burglary","Earthquake"], [0.95,0.94,0.29,0.001])
                 , ("JohnCalls", ["Alarm"], [0.9,0.05])
                 , ("MaryCalls", ["Alarm"], [0.7,0.02]) ]

---------------------------
-- Actually useful stuff --
---------------------------

bayes :: Ord a => Dist a -> Dist a
bayes = collect . normalize

condition :: Bool -> Dist ()
condition True  = return ()
condition False = D []
