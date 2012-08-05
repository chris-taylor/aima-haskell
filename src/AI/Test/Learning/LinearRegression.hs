module AI.Test.Learning.LinearRegression (runAllTests) where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.Container
import Test.QuickCheck

import AI.Learning.LinearRegression
import AI.Util.Matrix
import AI.Test.Util

-- |Regressing against a column of zeros should return a zero result vector.
testRegressionAgainstZeros :: Gen Bool
testRegressionAgainstZeros = do
    m <- choose (1,10)
    n <- choose (m+1,100)
    x <- arbitraryGaussianMatrix (n,m) :: Gen (Matrix Double)
    let y       = constant 0 n
        b       = constant 0 (m+1)
        bSample = regress y x
    return (bSample == b)


allTests =
    [ testRegressionAgainstZeros ]

runAllTests = mapM_ quickCheck allTests