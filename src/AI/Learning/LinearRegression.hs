{-# LANGUAGE FunctionalDependencies, FlexibleInstances, FlexibleContexts, BangPatterns #-}

module AI.Learning.LinearRegression where

import Data.List (foldl')
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra
import Prelude hiding (sum)

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
data LMOpts = LMOpts { lmIntercept :: Bool
                     , lmStandardize :: Bool }
                     deriving (Show)

-- |Standard options for linear regression (used by 'regress').
stdLMOpts :: LMOpts
stdLMOpts = LMOpts { lmIntercept = True
                   , lmStandardize = False }

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
                        , pReg :: Maybe t
                        , lmOpts :: LMOpts }
                        deriving (Show)

lm :: (Floating (Vector t), Field t) => Vector t -> Matrix t -> LinearModel t
lm = lmWith stdLMOpts

lmWith :: (Floating (Vector t), Field t) => LMOpts -> Vector t -> Matrix t -> LinearModel t
lmWith opts y x =
    let x1        = if lmStandardize opts then xs else x where (xs,_,_) = standardize x
        xx        = if lmIntercept opts then addOnes x1 else x1
        ybar      = constant (mean y) (dim y)
        beta      = regress xx y
        covBeta   = Nothing
        betaCI    = Nothing
        residuals = y - (xx <> beta)
        sst       = sumVector $ (y - ybar) ^ 2
        sse       = sumVector $ residuals ^ 2
        mse       = sse / fromIntegral (rows x)
        rSq       = 1 - sse / sst
        tBeta     = Nothing
        pBeta     = Nothing
        fReg      = Nothing
        pReg      = Nothing
    in LM beta covBeta betaCI residuals sst sse mse rSq tBeta pBeta fReg pReg opts

-------------------------------
-- Predict from linear model --
-------------------------------

lmPredict :: (Floating (Vector t), Field t) => LinearModel t -> Matrix t -> Vector t
lmPredict ( LM { beta = beta, lmOpts = opts } ) x = 
    let x1 = if lmStandardize opts then xs else x where (xs,_,_) = standardize x
        xx = if lmIntercept opts then addOnes x1 else x1
     in xx <> beta

------------------------
-- Perform Regression --
------------------------

-- |Regress a vector y against a matrix of predictors x, with the specified
--  options.
regress :: (Num (Vector t), Field t) =>
            Matrix t -- X
         -> Vector t -- y
         -> Vector t -- beta
regress x y
    | rows x /= dim y = error "Inconsistent dimensions -- REGRESS"
    | otherwise = let (_,n) = size x
                      (_,r) = qr x
                      rr = takeRows n r
                   in (trans rr <> rr) <\> trans x <> y

ridgeRegress :: (Num (Vector t), Field t) =>
                Matrix t -- X
             -> Vector t -- y
             -> t        -- lambda
             -> Bool     -- useConst
             -> Vector t -- beta
ridgeRegress x y lambda useConst
    | rows x /= dim y = error "Inconsistent dimensions -- RIDGEREGRESS"
    | otherwise = let (_,n) = size x
                      (_,r) = qr x
                      rr = takeRows n rr
                      ww = if useConst
                            then diag (fromList (0 : replicate (n-1) 1))
                            else ident n
                   in (trans rr <> rr + lambda `scale` ww) <\> trans x <> y

---------------
-- Utilities --
---------------

-- |Add a column of ones to a matrix.
addOnes :: (Num a, Container Vector a) => Matrix a -> Matrix a
addOnes x = horzcat [ones (rows x) 1, x]

-- |De-mean a sample.
demean :: (Num (Vector t), Field t) => Matrix t -> Matrix t
demean x = fromRows $ mapRows (subtract xbar) x
    where xbar = mean x

-- |Standardize a sample to have zero mean and unit variance.
standardize :: (Floating (Vector t), Fractional t, Element t) =>
               Matrix t                         -- Data sample
            -> (Matrix t, Vector t, Vector t)   -- Standardized data, mu, sigma
standardize m = (fromRows $ mapRows (\x -> (x - mu) / sigma) m, mu, sigma)
    where mu    = mean m
          sigma = std m

-------------------------
-- Mean, Variance etc. --
-------------------------

class Statistic a b | a -> b where
    mean :: a -> b
    sum :: a -> b
    var :: a -> b

std :: Floating b => Statistic a b => a -> b
std x = sqrt (var x)

instance Fractional t => Statistic [t] t where
    mean xs = xsum / xlen
        where (xsum,xlen)   = foldl' fun (0,0) xs
              fun (!a,!b) x = (a+x,b+1)
    sum xs = foldl' (+) 0 xs
    var xs = mean $ map ((^ 2) . subtract xbar) xs
        where
            xbar = mean xs

instance (Num (Vector t), Fractional t, Element t) => Statistic (Vector t) t where
    mean v = sumVector v / fromIntegral (dim v)
    sum v  = sumVector v
    var v  = mean $ (v - constant vbar (dim v)) ^ 2
        where vbar = mean v

instance (Num (Vector t), Fractional t, Element t) => Statistic (Matrix t) (Vector t) where
    mean m = fromList $ mapCols mean m
    sum m  = fromList $ mapCols sum m
    var m  = fromList $ mapCols var m


