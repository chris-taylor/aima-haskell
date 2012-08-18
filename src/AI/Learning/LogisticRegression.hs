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
lr y x = fst $ minimizeVD VectorBFGS2 prec niter sz1 tol cost grad theta0
    where
        prec    = 1e-9
        niter   = 1000
        sz1     = 0.1
        tol     = 0.1
        cost    = negate . fst . lrLogLikelihood y x -- negate because we call minimize
        grad    = negate . snd . lrLogLikelihood y x
        theta0  = constant 0 (cols x)

-- |Cost function and derivative for logistic regression. This is maximized when
--  fitting parameters for the regression.
--  simply the 
lrLogLikelihood :: Vector Double             -- targets (y)
               -> Matrix Double             -- design matrix (x)
               -> Vector Double             -- coefficients (theta)
               -> (Double, Vector Double)   -- (value, derivative)
lrLogLikelihood y x theta = (cost, grad)
    where
        m    = fromIntegral (rows x)    -- For computing average
        h    = sigmoid $ x <> theta     -- Predictions for y
        cost = sumVector (y * log h + (1-y) * log (1-h)) / m
        grad = (1/m) `scale` (y - h) <> x;

-- |Vectorized sigmoid function, sigmoid(x) = 1 / ( 1 + exp(-x) )
sigmoid :: Vector Double -> Vector Double
sigmoid v = sigmoid' `mapVector` v where sigmoid' x = 1 / (1 + exp (-x))

-------------
-- Testing --
-------------

test n k = do
    x <- randn n k                  -- design matrix
    e <- flatten `fmap` randn n 1   -- errors
    let theta = fromList $ 1 : replicate (k-1) 0
        h = sigmoid $ x <> theta + e
        y = (\i -> if i > 0.5 then 1 else 0) `mapVector` h
        theta_est = lr y x
    putStrLn $ "[y, h, x]"
    disp 3 $ takeRows 10 $ fromColumns [y, h] ! x
    putStrLn $ "[y, h, ypred]"
    disp 3 $ takeRows 10 $ fromColumns [y, h, sigmoid $ x <> theta_est]
    putStrLn $ "Number of observations: " ++ show n
    putStrLn $ "  Number of regressors: " ++ show k
    putStrLn $ "   Actual theta: " ++ show theta
    putStrLn $ "Estimated theta: " ++ show theta_est

