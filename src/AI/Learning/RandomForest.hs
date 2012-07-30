module AI.Learning.RandomForest where

import Control.Monad
import Control.Monad.Random

import AI.Learning.DecisionTree as D
import AI.Util.Util

newtype Forest a b = Forest [D.DTree a () b] deriving (Show)

randomForest :: (Ord a, Ord b, RandomGen g) =>
                Int         -- Number of trees in the forest
             -> Int         -- Number of attributes per tree
             -> (a -> b)    -- Target attribute
             -> [Att a]     -- Attributes to classify on
             -> [a]         -- 
             -> Rand g (Forest a b)
randomForest nTree nAtt target atts as = fmap Forest (replicateM nTree go)
    where
        go = do
            atts' <- selectMany nAtt atts
            as'   <- sampleWithReplacement n as
            return $ D.fitTree target atts' as'

        n = length as

decide :: Ord b => Forest a b -> a -> b
decide (Forest trees) a = mode $ map (`D.decide` a) trees