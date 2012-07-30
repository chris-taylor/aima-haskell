module AI.Learning.Core where

import Control.Monad.Random
import qualified Data.List as L

import AI.Util.Util

-------------
-- Testing --
-------------

type Builder a b = [a] -> [b] -> a -> b

-- |Compute the misclassification rate (MCR) of a particular decision tree
--  on a data set.
mcr :: Eq b =>
       (a -> b)     -- Classification algorithm
    -> [a]          -- List of elements to be classified
    -> [b]          -- List of correct classifications
    -> Float        -- Misclassification rate
mcr predfun as bs = 
    let bsPred     = map predfun as
        numCorrect = countIf id (zipWith (==) bs bsPred)
        numTotal   = length as
     in fromIntegral (numTotal - numCorrect) / fromIntegral numTotal

-- |Perform cross-validation with a training and test set, and return the
--  misclassification rate.
crossValidate :: Eq b =>
                   Builder a b  -- Classification function
                -> [a]          -- Training set observations
                -> [b]          -- Training set classifications
                -> [a]          -- Test set observations
                -> [b]          -- Test set classifications
                -> Float        -- Misclassification rate
crossValidate builder xTrain yTrain xTest yTest =
  let predfun = builder xTrain yTrain
   in mcr predfun xTest yTest

-----------------------------
-- K-Fold Cross-Validation --
-----------------------------

type CVPartition = [ ([Int], [Int]) ]

-- |Creates a set of training/test indexes for k-fold cross validation. If there
--  are @n@ elements in the list and @n = s * k + i@ then we want to return
--  @i@ test sets of size @s+1@ and @k-i@ test sets of size @s@.
cvPartition :: RandomGen g => Int -> Int -> Rand g CVPartition
cvPartition k n = do
    is <- go i (k - i) idx
    return $ map (\i -> (idx L.\\ i, i)) is
    where
        go 0 0 idx = return []

        go 0 j idx = do
            (is, idx') <- selectMany' s idx
            iss        <- go 0 (j-1) idx'
            return (is:iss)

        go i j idx = do
            (is, idx') <- selectMany' (s+1) idx
            iss        <- go (i-1) j idx'
            return (is:iss)

        s   = n `div` k
        i   = n `mod` k
        idx = [0 .. n-1]

-- |Perform k-fold cross-validation. Given a 'CVPartition' containing a list
--  of training and test sets, we repeatedly fit a model on the training set
--  and test its performance on the test set/
kFoldCV' :: Eq b =>
            CVPartition     -- Partition of training/test sets
         -> Builder a b     -- Model builder
         -> [a]             -- List of observations
         -> [b]             -- List of classifications
         -> Float           -- Average misclassification rate
kFoldCV' partition builder xs ys = go [] partition
    where
        go mcrs []           = mean mcrs
        go mcrs ((i1,i2):cs) =
            let xTrain = xs `elemsAt` i1
                yTrain = ys `elemsAt` i1
                xTest  = xs `elemsAt` i2
                yTest  = ys `elemsAt` i2
                mcr   = crossValidate builder xTrain yTrain xTest yTest
            in go (mcr:mcrs) cs

-- |Perform k-fold cross-validation, randomly generating the training and
--  test sets first.
kFoldCV :: (Eq b,RandomGen g) =>
           Int              -- The /k/ in k-fold cross-validation
        -> Builder a b      -- Model builder
        -> [a]              -- List of observations
        -> [b]              -- List of classifications
        -> Rand g Float     -- Misclassification rate
kFoldCV k builder xs ys = do
    cp <- cvPartition k (length xs)
    return (kFoldCV' cp builder xs ys)
