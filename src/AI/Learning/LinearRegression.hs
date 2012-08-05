module AI.Learning.LinearRegression where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra

import AI.Util.Matrix

data LROpts = LROpts { useConst :: Bool }

stdLROpts :: LROpts
stdLROpts = LROpts { useConst = True }

-- |Regress a vector y against a matrix of predictors x. Returns a coefficient
--  vector.
regress :: Field t => Vector t -> Matrix t -> Vector t
regress = regressWith stdLROpts

regressWith :: Field t => LROpts -> Vector t -> Matrix t -> Vector t
regressWith opts y x =
    let x1 = if useConst opts
                then addOnes x
                else x
        (_,m) = size x1
        (_,r) = qr x1
        r1 = takeRows m r
     in (trans r1 <> r1) <\> trans x1 <> y


---------------
-- Utilities --
---------------

addOnes :: (Num a, Element a) => Matrix a -> Matrix a
addOnes x = horzcat [ones (rows x, 1), x]