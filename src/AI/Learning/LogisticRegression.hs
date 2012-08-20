module AI.Learning.LogisticRegression where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Numeric.GSL.Minimization

import AI.Util.Matrix

-- |Multivariate logistic regression. Given a vector /y/ of target variables and
--  a design matrix /x/, this function fits the parameters /theta/ of a
--  logistic model, that is,
--
--  y = sigmoid( x_1 * theta_1 + ... + x_n * theta_n )
--
--  Typically the values in the vector /y/ are either boolean (i.e. 0/1) or they
--  represent frequency of observations, i.e. they are values between 0.0 and
--  1.0.
-- 
--  The function fits /theta/ by numerically maximizing the likelihood function.
--  It may be subject to overfit or non-convergence in the case where the number
--  of observations is small or the predictors are highly correlated.
lr :: Vector Double    -- targets (y)
   -> Matrix Double    -- design matrix (x)
   -> Vector Double    -- coefficient vector (theta)
lr y x = lrHelper (lrLogLikelihood y x) theta0
    where theta0  = constant 0 (cols x)

-- |Regularized logistic regression with quadratic penalty on the coefficients.
--  You should standardize the coefficients of the design matrix before using
--  this function, as the regularization procedure is sensitive to the scale
--  of the predictors. See also 'lr'.
lrRegularized :: Vector Double  -- targets (y)
              -> Matrix Double  -- design matrix (x)
              -> Bool           -- first column of design matrix is all ones?
              -> Double         -- regularization constant (lambda)
              -> Vector Double  -- coefficient vector (theta)
lrRegularized y x useConst lambda = lrHelper costfun theta0
    where
        costfun = lrLogLikRegularized y x useConst lambda
        theta0  = constant 0 (cols x)

-- |Helper function for logistic regressions. The first argument is a function
--  that returns the cost and gradient for a given vector of parameters, and
--  the second is the initial set of parameters to use.
lrHelper :: (Vector Double -> (Double, Vector Double)) -> Vector Double -> Vector Double
lrHelper fun theta0 = fst $ minimizeVD VectorBFGS2 prec niter sz1 tol cost grad theta0
    where
        prec    = 1e-9
        niter   = 1000
        sz1     = 0.1
        tol     = 0.1
        cost    = negate . fst . fun -- negate because we call minimize
        grad    = negate . snd . fun

-- |Cost function and derivative for logistic regression. This is maximized when
--  fitting parameters for the regression.
lrLogLikelihood :: Vector Double             -- targets (y)
                -> Matrix Double             -- design matrix (x)
                -> Vector Double             -- coefficients (theta)
                -> (Double, Vector Double)   -- (value, derivative)
lrLogLikelihood y x theta = (cost, grad)
    where
        m    = fromIntegral (rows x)    -- For computing average
        h    = sigmoid $ x <> theta     -- Predictions for y
        cost = sumVector (y * log h + (1-y) * log (1-h)) / m
        grad = (1/m) `scale` (y - h) <> x

-- |Cost function and derivative for regularized logistic regression. This is
--  maximized when fitting parameters for the regression.
lrLogLikRegularized :: Vector Double            -- targets (y)
                    -> Matrix Double            -- design matrix (x)
                    -> Bool                     -- is first col all ones?
                    -> Double                   -- regularization const (lambda)
                    -> Vector Double            -- coefficients (theta)
                    -> (Double, Vector Double)  -- (value, derivative)
lrLogLikRegularized y x useConst lambda theta = (cost, grad)
    where
        m = fromIntegral (rows x)
        (c,g)  = lrLogLikelihood y x theta
        theta' = if useConst then join [0, dropVector 1 theta] else theta
        cost   = c - (lambda / (2 * m)) * norm theta' ^ 2
        grad   = g - (lambda / m) `scale` theta'

-- |Vectorized sigmoid function, sigmoid(x) = 1 / ( 1 + exp(-x) )
sigmoid :: Vector Double -> Vector Double
sigmoid v = sigmoid' `mapVector` v where sigmoid' x = 1 / (1 + exp (-x))

-------------
-- Testing --
-------------

test n k lambda = do
    x <- randn n k                  -- design matrix
    e <- flatten `fmap` randn n 1   -- errors
    let theta = fromList $ 1 : replicate (k-1) 0
        h = sigmoid $ x <> theta + e
        y = (\i -> if i > 0.5 then 1 else 0) `mapVector` h
        theta_est1 = lr y x
        theta_est2 = lrRegularized y x False lambda
    --putStrLn $ "[y, h, x]"
    --disp 3 $ takeRows 10 $ fromColumns [y, h] ! x
    --putStrLn $ "[y, h, ypred]"
    --disp 3 $ takeRows 10 $ fromColumns [y, h, sigmoid $ x <> theta_est1]
    putStrLn $ "         Number of observations: " ++ show n
    putStrLn $ "           Number of regressors: " ++ show k
    putStrLn $ "                   Actual theta: " ++ show theta
    putStrLn $ "Estimated theta (unregularized): " ++ show theta_est1
    putStrLn $ "  Estimated theta (regularized): " ++ show theta_est2

