module AI.Learning.DecisionTree where

import Data.Map (Map, (!))
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Map as M

import AI.Util.Util

----------------
-- Attributes --
----------------

-- |An attribute is anything that can split the data into a number of classes.
data Att a = Att { test :: a -> Int
                 , label :: String }

instance Show (Att a) where
    show att = "Att(" ++ label att ++ ")"

-------------------
-- Decision Tree --
-------------------

-- |A decision tree which makes decisions based on attributes of type @a@ and
--  returns results of type @b@.
data DTree a b = Result b
               | Decision (Att a) (Map Int (DTree a b))

instance Show b => Show (DTree a b) where
    show (Result b) = show b
    show (Decision att ts) = 
        "Decision " ++ show att ++ " " ++ show (M.elems ts)

instance Functor (DTree a) where
    fmap f (Result b) = Result (f b)
    fmap f (Decision att branches) = Decision att (fmap (fmap f) branches)

instance Monad (DTree a) where
    return b = Result b
    Result b        >>= f = f b
    Decision att ts >>= f = Decision att (fmap (>>=f) ts)

-- |Create a decision tree from an attribute.
attribute :: (Enum b,Bounded b) => (a -> b) -> String -> DTree a b
attribute f label = Decision att tree
    where
        att  = Att (fromEnum . f) label
        tree = M.fromList $ zip [0..] (map Result enum)

-- |Run the decision tree on an example
decide :: DTree a b -> a -> b
decide (Result b) _ = b
decide (Decision att branches) a = decide (branches ! test att a) a

-- |Fit a decision tree to data, using the ID-3 algorithm.
fitTree :: (Ord a,Ord b) => (a -> b) -> [Att a] -> [a] -> DTree a b
fitTree target atts as = fmap mode $ decisionTreeLearning target atts [] as

----------------
-- Data Split --
----------------

-- |The decision-tree learning algorithm (Fig 18.5). This returns a list of
--  elements at each leaf. You can 'fmap' the 'mode' function over the leaves
--  to get the plurality value at that leaf, or the 'uniform' function to get
--  a probability distribution.
decisionTreeLearning :: Ord b =>
                        (a -> b)    -- Target function
                     -> [Att a]     -- Attributes to split on
                     -> [a]         -- Examples from the parent node
                     -> [a]         -- Examples to be split at this node
                     -> DTree a [b]
decisionTreeLearning target atts ps [] = Result (map target ps)
decisionTreeLearning target []   _  as = Result (map target as)
decisionTreeLearning target atts _  as
    | allEqual (map target as) = Result (map target as)
    | otherwise                = Decision att (fmap (decisionTreeLearning target atts' as) m)
        where
            (att,atts',m) =
                L.minimumBy (comparing (\(_,_,m) -> func (M.elems m))) choices

            choices =
                [ (att,atts',partition (test att) as) | (att,atts') <- points atts ]

            func = sumEntropy target

-- |Partition a list based on a function that maps elements of the list to
--  integers.
partition :: (a -> Int) -> [a] -> Map Int [a]
partition f xs = M.fromListWith (++) $ zip (map f xs) (map return xs)

-- |Compute the entropy of a list.
entropy :: Ord a => [a] -> Float
entropy as = negate (entropy' probs)
    where
        entropy' ps = sum $ map (\p -> if p == 0 then 0 else p * log p) ps
        probs       = map ((/len) . fromIntegral . length) $ L.group $ L.sort as
        len         = fromIntegral (length as)

-- |When given a target function, this can be used as an input to the 'minSplit'
--  routine.
sumEntropy :: Ord b => (a -> b) -> [[a]] -> Float
sumEntropy target as = sum $ map (entropy . map target) as

---------------
---- Pruning --
---------------

-- |Prune a tree to have a maximum depth of decisions.
--maxDecisions :: Int -> DTree a b -> DTree a b
--maxDecisions i (Decision att ts) =
--    if i == 0
--    then Result b
--    else Decision att $ fmap (maxDecisions (i-1)) ts
--maxDecisions _ r = r

---- |Prune decisions using a predicate.
--prune :: (b -> Bool) -> DTree a b -> DTree a b
--prune _ r@(Result b) = r
--prune p (Decision att b ts fs) = 
--    if p b
--    then Result b
--    else Decision att b (prune p ts) (prune p fs)
