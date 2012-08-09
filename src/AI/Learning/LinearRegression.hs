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
data LMOpts = LMOpts { fitIntercept :: Bool
                     , standardizeRegressors :: Bool }
                     deriving (Show)

-- |Standard options for linear regression (used by 'regress').
stdLMOpts = LMOpts { fitIntercept = True
                   , standardizeRegressors = False }

-------------------
-- Linear Models --
-------------------

data LMType = Normal | Ridge | Lasso deriving (Show)

data LinearModel t = LM { coefs   :: Vector t
                        , lmIntercept :: Bool
                        , lmStandardize :: Bool
                        , lmMean :: Vector t
                        , lmStd  :: Vector t }
                        deriving (Show)

data LMStats t = LMStats { covBeta :: Maybe (Matrix t)
                         , betaCI :: Maybe (Matrix t)
                         , sst :: t
                         , sse :: t
                         , mse :: t
                         , rSquare :: t
                         , tBeta :: Maybe (Vector t)
                         , pBeta :: Maybe (Vector t)
                         , fRegression :: Maybe t
                         , pRegression :: Maybe t }
                         deriving (Show)

lm :: (Floating (Vector t), Field t) => Matrix t -> Vector t -> LinearModel t
lm = lmWith stdLMOpts

lmWith :: (Floating (Vector t), Field t) => LMOpts -> Matrix t -> Vector t -> LinearModel t
lmWith opts x y = LM { coefs = beta
                     , lmIntercept = fitIntercept opts
                     , lmStandardize = standardizeRegressors opts
                     , lmMean = mu
                     , lmStd = sigma }
    where
        (xx,mu,sigma) = lmPrepare opts x
        beta          = regress xx y

lmPrepare :: (Floating (Vector t), Fractional t, Element t, Container Vector t) =>
             LMOpts
          -> Matrix t
          -> (Matrix t, Vector t, Vector t)
lmPrepare opts x = (x3,mu,sigma) 
    where
        (x1,mu,sigma) = standardize x
        x2            = if standardizeRegressors opts then x1 else x
        x3            = if fitIntercept opts then addOnes x2 else x2

-------------------------------
-- Predict from linear model --
-------------------------------

lmPredict :: (Floating (Vector t), Field t) => LinearModel t -> Matrix t -> Vector t
lmPredict model xx = x2 <> beta
    where
        beta    = coefs model
        xbar    = lmMean model
        sigma   = lmStd model
        x1      = if lmStandardize model
                    then eachRow (\x -> (x - xbar) / sigma) xx
                    else xx
        x2      = if lmIntercept model
                    then addOnes x1
                    else x1

lmStats :: (Floating (Vector t), Field t) => LinearModel t -> Matrix t -> Vector t -> LMStats t
lmStats model x y =
    let ybar      = constant (mean y) (dim y)
        yhat      = lmPredict model x
        residuals = y - yhat
        covBeta   = Nothing
        betaCI    = Nothing
        sst       = sumVector $ (y - ybar) ^ 2
        sse       = sumVector $ residuals ^ 2
        mse       = sse / fromIntegral (rows x)
        rSq       = 1 - sse / sst
        tBeta     = Nothing
        pBeta     = Nothing
        fReg      = Nothing
        pReg      = Nothing
    in LMStats covBeta betaCI sst sse mse rSq tBeta pBeta fReg pReg

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
demean x = eachRow (subtract $ mean x) x

-- |Standardize a sample to have zero mean and unit variance.
standardize :: (Floating (Vector t), Fractional t, Element t) =>
               Matrix t                         -- Data sample
            -> (Matrix t, Vector t, Vector t)   -- Standardized data, mu, sigma
standardize m = (eachRow (\x -> (x - mu) / sigma) m, mu, sigma)
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


