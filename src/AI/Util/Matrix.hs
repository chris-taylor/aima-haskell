{-# LANGUAGE FlexibleContexts #-}

module AI.Util.Matrix where

import Foreign.Storable (Storable)
import Numeric.LinearAlgebra

-- |Create an @rxc@ matrix of zeros.
zeros :: (Num a, Container Vector a)  => Int -> Int -> Matrix a
zeros r c = konst 0 (r,c)

-- |Create an @rxc@ matrix of ones.
ones :: (Num a, Container Vector a) => Int -> Int -> Matrix a
ones r c = konst 1 (r,c)

-- |Return the size of a matrix as a 2-tuple.
size :: Matrix a -> (Int,Int)
size x = (rows x, cols x)

-- |Concatenate matrices horizontally.
horzcat :: Element a => [Matrix a] -> Matrix a
horzcat = fromBlocks . return

-- |Concatenate matrices vertically.
vertcat :: Element a => [Matrix a] -> Matrix a
vertcat = fromBlocks . map return

--------------------------
-- Functions on Vectors --
--------------------------

sumVector :: (Num a, Storable a) => Vector a -> a
sumVector xs = foldVector (+) 0 xs

prodVector :: (Num a, Storable a) => Vector a -> a
prodVector xs = foldVector (*) 1 xs

---------------------------
-- Functions on Matrices --
---------------------------

mapRows :: Element a => (Vector a -> b) -> Matrix a -> [b]
mapRows f m = map f (toRows m)

mapCols :: Element a => (Vector a -> b) -> Matrix a -> [b]
mapCols f m = map f (toColumns m)

eachRow :: (Element a, Element b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
eachRow f = fromRows . mapRows f

eachCol :: (Element a, Element b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
eachCol f = fromColumns . mapCols f

