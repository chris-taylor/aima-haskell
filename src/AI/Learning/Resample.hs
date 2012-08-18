module AI.Learning.Resample where

import Control.Monad.Random
import Foreign.Storable (Storable)
import Numeric.LinearAlgebra
import qualified Data.List as L

import AI.Util.Matrix
import AI.Util.Util

---------------
-- Bootstrap --
---------------

-- |Generate a bootstrap sample of size @sz@.
genBootstrapSample :: RandomGen g => Int -> Rand g [Int]
genBootstrapSample sz = go sz []
    where go 0 accum = return accum
          go n accum = do
            i <- getRandomR (0,sz-1)
            go (n - 1) (i:accum)

sampleVector :: (Storable a, RandomGen g) => Vector a -> Rand g (Vector a)
sampleVector v = do
    idx <- genBootstrapSample (dim v)
    return (v `subRefVec` idx)

sampleMatrixRows :: (Element a, RandomGen g) => Matrix a -> Rand g (Matrix a)
sampleMatrixRows m = do
    idx <- genBootstrapSample (rows m)
    return $ m `subRefRows` idx

sampleMatrixCols :: (Element a, RandomGen g) => Matrix a -> Rand g (Matrix a)
sampleMatrixCols m = do
    idx <- genBootstrapSample (cols m)
    return $ m `subRefCols` idx

-- |Generate a bootstrap sample of a statistic from a data set of type /a/.
bootStrapResample :: RandomGen g =>
                     (a -> Rand g a)    -- Sampling function
                  -> (a -> b)           -- Statistic to be resamples
                  -> Int                -- Number of resamples
                  -> a                  -- Data
                  -> Rand g [b]
bootStrapResample sample func nSamples x = go [] nSamples
    where
        go samples 0 = return samples
        go samples n = do
            x' <- sample x
            go (func x' : samples) (n-1)

----------------------
-- Cross Validation --
----------------------

-- |Type for cross-validation partitions.
data CVPartition = CVPartition [ ([Int], [Int]) ]

-- |Specify what type of cross-validation you want to do.
data CVType = LeaveOneOut
            | KFold Int

-- |Create a partition into test and training sets.
cvPartition :: RandomGen g => Int -> CVType -> Rand g CVPartition
cvPartition sz cvtype = case cvtype of
    KFold i     -> cvp sz i
    LeaveOneOut -> cvp sz sz

-- |Helper function for 'cvPartition'.
cvp :: RandomGen g => Int -> Int -> Rand g CVPartition
cvp n k = do
    is <- go i (k - i) idx
    return . CVPartition $ map (\i -> (idx L.\\ i, i)) is
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
    
-- |Prediction function. A prediction function should take a training and a test
--  set, and use the training set to build a model whose performance is
--  evaluated on the test set, returning a final score as a 'Double'.
type PredFun = Matrix Double    -- Training set regressors
            -> Vector Double    -- Training set target
            -> Matrix Double    -- Test set regressors
            -> Vector Double    -- Test set target
            -> Double           -- Performance score

-- |Model builder. A model builder takes a training set of regressors and
--  targets, and constructs a function that makes predictions from an out-
--  of-sample set of regressors.
type ModelBuilder = Matrix Double   -- Training set regressors
                 -> Vector Double   -- Training set target
                 -> Matrix Double   -- Out-of-sample regressors
                 -> Vector Double   -- Predictions

-- |Evaluation function. An evaluation function takes a vector of targets and
--  a vector of predictions, and returns a score corresponding to how closely
--  the predictions match the target.
type EvalFun = Vector Double    -- Target
            -> Vector Double    -- Predictions
            -> Double           -- Score (e.g. MSE, MCR, likelihood)

-- |Perform k-fold cross-validation. Given a 'CVPartition' containing a list
--  of training and test sets, we repeatedly fit a model on the training set
--  and test its performance on the test set/
kFoldCV_ :: CVPartition
         -> PredFun
         -> Matrix Double
         -> Vector Double
         -> [Double]
kFoldCV_ (CVPartition partition) predfun x y = L.foldl' go [] partition
    where
        go scores (trainIdx,testIdx) =
            let xTrain = x `subRefRows` trainIdx
                yTrain = y `subRefVec`  trainIdx
                xTest  = x `subRefRows` testIdx
                yTest  = y `subRefVec`  testIdx
                score  = predfun xTrain yTrain xTest yTest
            in score:scores

-- |Perform k-fold cross-validation, randomly generating the training and
--  test sets first.
kFoldCV :: RandomGen g =>
           CVType           -- What type of cross-validation?
        -> PredFun          -- Prediction function
        -> Matrix Double    -- Regressors
        -> Vector Double    -- Targets
        -> Rand g [Double]  -- List of scores
kFoldCV cvtype predfun x y = do
    if rows x /= dim y
        then error "Inconsistent dimensions -- KFOLDCV"
        else do
            cp <- cvPartition (rows x) cvtype
            return (kFoldCV_ cp predfun x y)
