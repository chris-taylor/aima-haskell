module AI.Learning.LinearRegression where

import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra

--regress :: Floating a 
regress y x = inv (trans x <> x) <> trans x <> y

