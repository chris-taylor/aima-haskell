module AI.Learning.Perceptron where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

perceptronPredict :: Vector Double -> Matrix Double -> Vector Double
perceptronPredict weights x = step (x <> weights)

perceptronCost :: Vector Double -> Vector Double -> Vector Double -> Double
perceptronCost y yhat = undefined

----------------------
-- Gradient Descent --
----------------------

gradientDescent :: (Vector Double -> Vector Double) -- f
                -> Vector Double                    -- x0
                -> Double                           -- alpha
                -> Double                           -- tol
                -> Vector Double
gradientDescent g x0 alpha tol = go x0 (fun x0)
    where
        go x x' = if converged x x'
                    then x'
                    else go x' (fun x')
        converged a b = norm b / norm a - 1 < tol
        fun x = gradientDescentStep g alpha x

gradientDescentStep :: (Vector Double -> Vector Double) -> Double -> Vector Double -> Vector Double
gradientDescentStep g alpha x = x - alpha `scale` g x
