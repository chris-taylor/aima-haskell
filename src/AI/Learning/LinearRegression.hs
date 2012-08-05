{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module AI.Learning.LinearRegression where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra

import AI.Util.Matrix

{-

A linear model should contain the following information:

* Regression coefficients
* Covariance matrix of regression coefficients
* Residuals
* Confidence intervals for regression coefficients
* Confidence intervals for residuals
* T-Statistic for regression coefficients
* P-Values for regression coefficients
* F-Statistic for regression
* P-Value for regression
* MSE
* SST
* SSE
* PRESS Statistic
* R squared
* adjusted r squared
* 

-}

-------------
-- Options --
-------------

-- |Data type for linear regression options.
data LMOpts = LMOpts { useConst :: Bool }

-- |Standard options for linear regression (used by 'regress').
stdLMOpts :: LMOpts
stdLMOpts = LMOpts { useConst = True }

-------------------
-- Linear Models --
-------------------

data LinearModel t = LM { beta :: Vector t
                        , covBeta :: Maybe (Matrix t)
                        , betaCI :: Maybe (Matrix t)
                        , residuals :: Vector t
                        , sst :: t
                        , sse :: t
                        , mse :: t
                        , rSq :: t
                        , tBeta :: Maybe (Vector t)
                        , pBeta :: Maybe (Vector t)
                        , fReg :: Maybe t
                        , pReg :: Maybe t }
                        deriving (Show)

lm :: (Floating t, Field t, Element t) => Vector t -> Matrix t -> LinearModel t
lm = lmWith stdLMOpts

lmWith :: (Floating t, Field t, Element t) => LMOpts -> Vector t -> Matrix t -> LinearModel t
lmWith opts y x =
    let x1        = if useConst opts then addOnes x else x
        ybar      = mean y
        beta      = regress y x1
        covBeta   = Nothing
        betaCI    = Nothing
        residuals = zipVectorWith (-) y (x1 <> beta)
        sst       = sumVector $ mapVector ((^2) . subtract ybar) y
        sse       = sumVector $ mapVector (^2) residuals
        mse       = sse / fromIntegral (rows x)
        rSq       = 1 - sse / sst
        tBeta     = Nothing
        pBeta     = Nothing
        fReg      = Nothing
        pReg      = Nothing
    in LM beta covBeta betaCI residuals sst sse mse rSq tBeta pBeta fReg pReg


------------------------
-- Perform Regression --
------------------------

-- |Regress a vector y against a matrix of predictors x, with the specified
--  options.
regress :: (Field t, Floating t) => Vector t -> Matrix t -> Vector t
regress y x =
    let (_,m) = size x
        (_,r) = qr x
        r' = takeRows m r
     in (trans r' <> r') <\> trans x <> y

---------------
-- Utilities --
---------------

-- Add a column of ones to a matrix.
addOnes :: (Num a, Element a) => Matrix a -> Matrix a
addOnes x = horzcat [ones (rows x, 1), x]

class Mean a b | a -> b where
    mean :: a -> b

instance (Floating t, Element t) => Mean (Vector t) t where
    mean v = sumVector v / fromIntegral (dim v)

instance (Floating t, Element t) => Mean (Matrix t) (Vector t) where
    mean m = fromList $ map mean $ toColumns m

class Cov a b | a -> b where
    cov :: a -> b



