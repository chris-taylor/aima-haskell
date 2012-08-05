module AI.Test.Util where

import Data.Packed.Vector
import Data.Packed.Matrix
import Foreign.Storable
import Numeric.Container
import Test.QuickCheck hiding ((><))

arbitraryGaussianVector :: Int -> Gen (Vector Double)
arbitraryGaussianVector n = do
    seed <- arbitrary
    return (randomVector seed Gaussian n)

arbitraryUniformVector :: Int -> Gen (Vector Double)
arbitraryUniformVector n = do
    seed <- arbitrary
    return (randomVector seed Uniform n)

arbitraryGaussianMatrix :: (Int,Int) -> Gen (Matrix Double)
arbitraryGaussianMatrix (n,m) = do
    seed  <- arbitrary
    let mu  = constant 0 m
        cov = ident m
    return (gaussianSample seed n mu cov)

arbitraryUniformMatrix :: (Int,Int) -> Gen (Matrix Double)
arbitraryUniformMatrix (n,m) = do
    seed <- arbitrary
    return (uniformSample seed n (replicate m (0,1)))

--instance (Storable a, Arbitrary a) => Arbitrary (Matrix a) where
--    arbitrary = do
--        n  <- arbitrary `suchThat` \n -> n > 0 && n < 10
--        m  <- arbitrary `suchThat` \m -> m > 0 && m < 100
--        s  <- arbitrary

--        return $ (n><m) xs