module AI.Learning.RandomForest where

import Control.Monad
import Control.Monad.Random

import AI.Learning.DecisionTree as D
import AI.Util.Util

-- |A forest is a list of decision trees.
newtype Forest a b = Forest [D.DTree a () b] deriving (Show)

-- |Create a new random forest. This function repeatedly selects a random
--  subset of the attributes to split on, and fits an unpruned tree using
--  a bootstrap sample of the observations. This is repeated many times,
--  creating a /forest/ of decision trees.
randomForest :: (Ord a, Ord b, RandomGen g) =>
                Int         -- Number of trees in the forest
             -> Int         -- Number of attributes per tree
             -> (a -> b)    -- Target attribute
             -> [Att a]     -- Attributes to classify on
             -> [a]         -- List of observations
             -> Rand g (Forest a b)
randomForest nTree nAtt target atts as = fmap Forest (replicateM nTree go)
    where
        go = do atts' <- selectMany nAtt atts
                as'   <- sampleWithReplacement (length as) as
                return $ D.fitTree target atts' as'

-- |Use a forest to classify a new example. We run the example through each
--  of the trees in the forest, and choose the most common classification.
decide :: Ord b => Forest a b -> a -> b
decide (Forest trees) a = mode $ map (`D.decide` a) trees