module AI.Learning.CrossValidation where

import Control.Monad.Random
import Foreign.Storable (Storable)
import Numeric.LinearAlgebra
import qualified Data.List as L

import AI.Util.Matrix
import AI.Util.Util

----------------------
-- Cross Validation --
----------------------

class Indexable c where
    index :: c -> Index -> c
    nobs :: c -> Int

instance Storable a => Indexable (Vector a) where
    index = subRefVec
    nobs = dim

instance Element a => Indexable (Matrix a) where
    index = subRefRows
    nobs = rows

instance Indexable [a] where
    index = map . (!!) 
    nobs = length

-- |Indexes are lists of 'Int'. Should refactor this to use something more
--  efficient.
type Index  = [Int]

-- |Type for cross-validation partition.
data CVPartition = CVPartition [(Index, Index)]

-- |Specify what type of cross-validation you want to do.
data CVType = LeaveOneOut
            | KFold Int
    
-- |Prediction function. A prediction function should take a training and a test
--  set, and use the training set to build a model whose performance is
--  evaluated on the test set, returning a final score as a 'Double'.
type PredFun a b = a        -- Training set predictors
                -> b        -- Training set target
                -> a        -- Test set predictors
                -> b        -- Test set target
                -> Double   -- Performance score

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

-- |Perform k-fold cross-validation. Given a 'CVPartition' containing a list
--  of training and test sets, we repeatedly fit a model on the training set
--  and test its performance on the test set/
kFoldCV_ :: (Indexable a, Indexable b) => 
            CVPartition
         -> PredFun a b
         -> a
         -> b
         -> [Double]
kFoldCV_ (CVPartition partition) predfun x y = map go partition
    where
        go (trainIdx,testIdx) = predfun xTrain yTrain xTest yTest
            where
                xTrain = x `index` trainIdx
                yTrain = y `index` trainIdx
                xTest  = x `index` testIdx
                yTest  = y `index` testIdx

-- |Perform k-fold cross-validation, randomly generating the training and
--  test sets first.
kFoldCV :: (RandomGen g, Indexable a, Indexable b) =>
           CVType           -- What type of cross-validation?
        -> PredFun a b      -- Prediction function
        -> a                -- Predictors
        -> b                -- Targets
        -> Rand g [Double]  -- List of scores
kFoldCV cvtype predfun x y = if nobs x /= nobs y
    then error "Inconsistent dimensions -- KFOLDCV"
    else do
        cp <- cvPartition (nobs x) cvtype
        return (kFoldCV_ cp predfun x y)

---------------
-- Old Stuff --
---------------

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