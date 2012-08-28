module AI.Learning.Core where

import Numeric.LinearAlgebra
import Numeric.GSL.Minimization

---------------
-- Utilities --
---------------

-- |Sigmoid function:
--
--  > sigmoid x = 1 / (1 + exp (-x))
--
--  Used in the logistic regression and neural network modules.
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

------------------
-- Optimization --
------------------

-- |Simplified minimization function. You supply functions that compute the
--  quantity to be minimized and its gradient, and an initial guess, and the
--  final solution is returned.
minimizeS :: (Vector Double -> Double)          -- f
          -> (Vector Double -> Vector Double)   -- gradient
          -> Vector Double                      -- initial x
          -> Vector Double
minimizeS f g x = fst $ minimizeVD VectorBFGS2 prec niter sz tol f g x
    where prec    = 1e-9
          niter   = 1000
          sz      = 0.1
          tol     = 0.1
