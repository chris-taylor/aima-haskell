{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module AI.Learning.DecisionTree where

import Control.Monad.Random
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
                 , vals :: [Int]
                 , label :: String }

instance Show (Att a) where
    show att = "Att(" ++ label att ++ ")"

instance Eq (Att a) where
    Att _ _ lab1 == Att _ _ lab2 = lab1 == lab2

-------------------
-- Decision Tree --
-------------------

-- |A decision tree which makes decisions based on attributes of type @a@ and
--  returns results of type @b@. We store information of type @i@ at the nodes,
--  which is useful for pruning the tree later.
data DTree a i b = Result b
                 | Decision (Att a) i (Map Int (DTree a i b))

instance Show b => Show (DTree a i b) where
    show (Result b) = show b
    show (Decision att _ ts) = 
        "Decision " ++ show att ++ " " ++ show (M.elems ts)

instance Functor (DTree a i) where
    fmap f (Result b) = Result (f b)
    fmap f (Decision att i branches) = Decision att i (fmap (fmap f) branches)

instance Monad (DTree a i) where
    return b = Result b
    Result b          >>= f = f b
    Decision att i ts >>= f = Decision att i (fmap (>>=f) ts)

mapI :: (i -> j) -> DTree a i b -> DTree a j b
mapI f (Result b) = Result b
mapI f (Decision att i ts) = Decision att (f i) (fmap (mapI f) ts)

dropInfo :: DTree a i b -> DTree a () b
dropInfo = mapI $ const ()

-- |Create an attribution from a function and its name.
att :: forall a b. (Enum b, Bounded b) => (a -> b) -> String -> Att a
att f str = Att (fromEnum . f) (map fromEnum vs) str
    where
        vs = enum :: [b]

-- |Create a simple decision tree from a function.
attribute :: (Enum b,Bounded b) => (a -> b) -> String -> DTree a () b
attribute f label = Decision (att f label) () tree
    where
        tree = M.fromList $ zip [0..] (map Result enum)

-- |Run the decision tree on an example
decide :: DTree a i b -> a -> b
decide (Result b) _ = b
decide (Decision att _ branches) a = decide (branches ! test att a) a

-- |Fit a decision tree to data, using the ID-3 algorithm.
fitTree :: (Ord a,Ord b) => (a -> b) -> [Att a] -> [a] -> DTree a () b
fitTree target atts as =
    dropInfo $ fmap mode $ decisionTreeLearning target atts [] as

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
                     -> DTree a [b] [b]
decisionTreeLearning target atts ps as
    | null as                    = Result ps'
    | null atts  || allEqual as' = Result as'
    | otherwise                  =
        Decision att as' (fmap (decisionTreeLearning target atts' as) m)
        where
            (att,atts',m) =
                L.minimumBy (comparing (\(_,_,m) -> func (M.elems m))) choices

            choices =
                [ (att,atts',partition att as) | (att,atts') <- points atts ]

            func = sumEntropy target

            as' = map target as
            ps' = map target ps

-- |Partition a list based on a function that maps elements of the list to
--  integers. This assumes that
partition :: Att a -> [a] -> Map Int [a]
partition att as = L.foldl' fun initial as
    where
        fun m a = M.insertWith' (++) (test att a) [a] m
        initial = mkUniversalMap (vals att) []

entropy :: Ord a => [a] -> Float
entropy as = entropy' probs
    where
        entropy' ps = negate . sum $ map (\p -> if p == 0 then 0 else p * log p) ps
        probs    = map ((/len) . fromIntegral) $ M.elems $ L.foldl' go M.empty as
        go m a = M.insertWith' (const (+1)) a 1 m
        len      = fromIntegral (length as)

-- |When given a target function, this can be used as an input to the 'minSplit'
--  routine.
sumEntropy :: Ord b => (a -> b) -> [[a]] -> Float
sumEntropy target as = sum $ map (entropy . map target) as

-------------
-- Pruning --
-------------

-- |Prune a tree to have a maximum depth of decisions.
maxDecisions :: Int -> DTree a b b -> DTree a b b
maxDecisions i (Decision att as ts) =
    if i == 0
    then Result as
    else Decision att as $ fmap (maxDecisions (i-1)) ts
maxDecisions _ r = r

-- |Prune decisions using a predicate.
prune :: (b -> Bool) -> DTree a b b -> DTree a b b
prune _ (Result b)          = Result b
prune p (Decision att i ts) = 
    if p i
    then Result i
    else Decision att i (fmap (prune p) ts)

-------------
-- Testing --
-------------

-- |Compute the misclassification rate (MCR) of a particular decision tree
--  on a data set.
mcr :: Eq b =>
       (a -> b)     -- Classification algorithm
    -> [a]          -- List of elements to be classified
    -> [b]          -- List of correct classifications
    -> Double       -- Misclassification rate
mcr predfun as bs = 
    let bsPred     = map predfun as
        numCorrect = countIf id (zipWith (==) bs bsPred)
        numTotal   = length as
     in fromIntegral (numTotal - numCorrect) / fromIntegral numTotal

predfun xtrain ytrain xtest ytest = undefined