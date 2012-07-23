-- |The Bayes Net module makes heavy use of one-dimensional structures to store
--  N-dimensional arrays of numbers. This requires clever use of indexing when
--  they need to be sliced and subsetted. This module is a convenient place to
--  store the functions that do this.
module AI.Util.Array where

import qualified Data.List as L

-- |Given a list of 'Int' in [0,1] that index an n-dimensional array, convert it
--  to a single 'Int' that represents that index in a 1-d array.
ndSubRef :: [Int] -> Int
ndSubRef = L.foldl' (\a d -> 2 * a + d) 0

-- |Sub-reference a list. Return the sublist consisting of those elements at
--  the specified indexes.
elemsAt :: [a] -> [Int] -> [a]
elemsAt xs ns = map (xs!!) ns

-- |Return the index of the first occurence of a particular element in a list.
indexOf :: Eq a => [a] -> a -> Int
indexOf xs x = case L.elemIndex x xs of
    Nothing -> error "Element not found -- INDEXOF"
    Just i  -> i

-- |Given a list of variables and a list of fixings, return a list (index,value)
--  which can be used to 'subSlice' a conditional probability vector.
getIxVector :: Eq e => [e] -> [(e,Bool)] -> [(Int,Bool)]
getIxVector vars []           = []
getIxVector vars ((v,x):rest) = if v `elem` vars
    then (vars `indexOf` v, x) : getIxVector vars rest
    else getIxVector vars rest

-- |This function returns the indexes to take an (n-1)-dimensional subslice of
--  an n-dimensional array. The first argument gives n, the number of
--  dimensions of the array. The second argument gives the index being fixed.
--  The indexes returned can be used to index into an array of length 2^(n-1).
subSliceIdx :: Int -> (Int, Bool) -> [Int]
subSliceIdx ndim (i,x) = filter f [0 .. 2^ndim - 1]
    where
        f   = if x then select else not . select
        select n = (n `div` 2 ^ (ndim - i - 1)) `mod` 2 == 0

-- |This function returns the indexes to perform an arbitrary subslice of an
--  n-dimensional array, by fixing a subset of its indexes.
subSliceIdxs :: Int -> [(Int,Bool)] -> [Int]
subSliceIdxs ndim []   = [0 .. 2^ndim - 1]
subSliceIdxs ndim idxs = L.foldl1' L.intersect $ map (subSliceIdx ndim) idxs

-- |Given a 1-dimensional array storing the values of an N-dimensional array,
--  take a subslice by fixing one of the dimensions to either 0 or 1.
subSlice1 :: [a] -> (Int,Bool) -> [a]
subSlice1 xs (i,x) = xs `elemsAt` subSliceIdx (log2 $ length xs) (i,x)

-- |Given a 1-dimensional array storing the values of an N-dimensional array,
--  take a subslice by fixing a subset of the indexes to either 0 or 1.
subSlice :: [a] -> [(Int,Bool)] -> [a]
subSlice xs is = xs `elemsAt` subSliceIdxs (log2 $ length xs) is

-- |Base 2 logarithm for 'Int's.
log2 :: Int -> Int
log2 n = go n 0 where go n x = if n == 1 then x else go (n `div` 2) (x+1)

