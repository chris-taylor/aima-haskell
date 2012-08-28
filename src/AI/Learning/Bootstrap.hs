module AI.Learning.Bootstrap where

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
                  -> (a -> b)           -- Statistic to be resampled
                  -> Int                -- Number of resamples
                  -> a                  -- Data
                  -> Rand g [b]
bootStrapResample sample func nSamples x = go [] nSamples
    where
        go samples 0 = return samples
        go samples n = do
            x' <- sample x
            go (func x' : samples) (n-1)
