{-# LANGUAGE FunctionalDependencies, FlexibleInstances, FlexibleContexts, BangPatterns #-}

module AI.Learning.LinearRegression where

import Data.List (foldl')
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util (ones)

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

-- |Type recording what kind of linear model we want to build. Choose from
--  ordinary least squares (OLS), ridge regression with parameter lambda
--  and LASSO with parameter lambda.
data LMType = OLS
            | Ridge Double
            | LASSO Double
            deriving (Show)

---- |Data type for linear regression options.
data LMOpts = LMOpts { fitIntercept :: Bool
                     , standardizeRegressors :: Bool }
                     deriving (Show)

---- |Standard options for linear regression (used by 'regress').
stdLMOpts = LMOpts { fitIntercept = True
                   , standardizeRegressors = False }

---------------------
---- Linear Models --
---------------------

-- |Data type for a linear model. It consists of the coefficient vector,
--  together with options that specify how to transform any data that is used
--  to make new predictions.
data LinearModel = LM { coefs   :: Vector Double
                      , lmIntercept :: Bool
                      , lmStandardize :: Bool
                      , lmMean :: Vector Double
                      , lmStd  :: Vector Double }
                      deriving (Show)

-- |Statistics structure for a linear regression.
data LMStats = LMStats { covBeta :: Maybe (Matrix Double)
                       , betaCI :: Maybe (Matrix Double)
                       , sst :: Double
                       , sse :: Double
                       , mse :: Double
                       , rSquare :: Double
                       , tBeta :: Maybe (Vector Double)
                       , pBeta :: Maybe (Vector Double)
                       , fRegression :: Maybe Double
                       , pRegression :: Maybe Double }
                       deriving (Show)

type Predictor = Matrix Double
type Response = Vector Double

-- |Fit an ordinary least squares linear model to data.
lm :: Matrix Double -> Vector Double -> LinearModel
lm = lmWith OLS stdLMOpts

-- |Fit a ridge regression (least squares with quadratic penalty) to data.
lmRidge :: Matrix Double -> Vector Double -> Double -> LinearModel
lmRidge x y lambda = lmWith (Ridge lambda) stdLMOpts x y

-- |Fit a linear model to data. The exact model fitted is specified by the
--  'LMType' argument, and the options are specified in 'LMOpts'.
lmWith :: LMType -> LMOpts -> Matrix Double -> Vector Double -> LinearModel
lmWith kind opts x y = LM { coefs = beta
                          , lmIntercept = fitIntercept opts
                          , lmStandardize = standardizeRegressors opts
                          , lmMean = mu
                          , lmStd = sigma }
    where
        (xx,mu,sigma) = lmPrepare opts x
        beta = case kind of
            OLS     -> regress xx y
            Ridge a -> ridgeRegress xx y (fitIntercept opts) a
            LASSO a -> error "LASSO not implemented."

-- |Prepare data according to the specified options structure. This may involve
--  centering and standardizing the data, or adding a column of constants.
lmPrepare :: LMOpts
          -> Matrix Double
          -> (Matrix Double, Vector Double, Vector Double)
lmPrepare opts x = (x3,mu,sigma) 
    where
        (x1,mu,sigma) = standardize x
        x2            = if standardizeRegressors opts then x1 else x
        x3            = if fitIntercept opts then addOnes x2 else x2

---------------------------------
---- Predict from linear model --
---------------------------------

-- |Make predictions from a linear model.
lmPredict :: LinearModel -> Matrix Double -> Vector Double
lmPredict model x = x2 <> beta
    where
        beta    = coefs model
        xbar    = lmMean model
        sigma   = lmStd model
        x1      = if lmStandardize model
                    then eachRow (\x -> (x - xbar) / sigma) x
                    else x
        x2      = if lmIntercept model
                    then addOnes x1
                    else x1

-- |Calculate statistics for a linear regression.
lmStats :: LinearModel -> Matrix Double -> Vector Double -> LMStats
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

mseEvalFun :: Response -> Response -> Double
mseEvalFun actual predicted = mean $ (actual - predicted) ^ 2

lmPredFun :: Predictor -> Response -> Predictor -> Response -> Double
lmPredFun xtrain ytrain xtest ytest = mseEvalFun ytest ypred
    where
        ypred = lmPredict model xtest
        model = lm xtrain ytrain

--------------------------
---- Perform Regression --
--------------------------

---- |Regress a vector y against a matrix of predictors x, with the specified
----  options.
regress :: Matrix Double -- X
        -> Vector Double -- y
        -> Vector Double -- beta
regress x y
    | rows x /= dim y = error "Inconsistent dimensions -- REGRESS"
    | otherwise = let (_,n) = size x
                      (_,r) = qr x
                      rr = takeRows n r
                   in (trans rr <> rr) <\> trans x <> y

-- |Ridge regression. This adds a penalty term to OLS regression, which
--  discourages large coefficient values to prevent overfitting.
ridgeRegress :: Matrix Double  -- X
             -> Vector Double  -- y
             -> Bool           -- useConst?
             -> Double         -- lambda
             -> Vector Double  -- beta
ridgeRegress x y useConst lambda
    | rows x /= dim y = error "Inconsistent dimensions -- RIDGEREGRESS"
    | otherwise = let (_,n) = size x
                      (_,r) = qr x
                      rr = takeRows n r
                      ww = if useConst
                            then diag $ join [0, constant 1 (n-1)]
                            else ident n
                   in (trans rr <> rr + lambda `scale` ww) <\> trans x <> y

-----------------
---- Utilities --
-----------------

-- |De-mean a sample.
demean :: Matrix Double -> Matrix Double
demean x = eachRow (subtract $ mean x) x

-- |Standardize a sample to have zero mean and unit variance. Returns a triple
--  consisting of the standardized sample, the sample mean and the sample
--  standard deviation.
standardize :: Matrix Double -> (Matrix Double, Vector Double, Vector Double)
standardize m = (eachRow (\x -> (x - mu) / sigma) m, mu, sigma)
    where mu    = mean m
          sigma = std m

-- |Standardize a sample to have zero mean and unit variance. This variant of
--  the function discards the mean and standard deviation vectors, only
--  returning the standardized sample.
standardize_ :: Matrix Double -> Matrix Double
standardize_ x = a where (a,_,_) = standardize x 

-------------------------
-- Mean, Variance etc. --
-------------------------

class Mean a b | a -> b where
    mean :: a -> b

class Floating b => Variance a b | a -> b where
    var :: a -> b
    var x = y * y where y = std x

    std :: a -> b
    std x = sqrt $ var x

instance Mean (Vector Double) Double where
    mean v = sumVector v / fromIntegral (dim v)

instance Variance (Vector Double) Double where
    var v  = mean $ (v - constant vbar (dim v)) ^ 2
        where vbar = mean v

instance Mean (Matrix Double) (Vector Double) where
    mean m = fromList $ mapCols mean m

instance Variance (Matrix Double) (Vector Double) where
    var m  = fromList $ mapCols var m


