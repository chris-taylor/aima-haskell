module AI.Util.Matrix where

import Foreign.Storable (Storable)
import Data.Packed.Matrix
import Data.Packed.Vector

-- |Create an @nxm@ matrix with every element identical.
constantMatrix :: Storable a => (Int,Int) -> a -> Matrix a
constantMatrix (n,m) x = (n><m) (repeat x)

-- |Create an @nxm@ matrix of zeros.
zeros :: (Num a, Storable a) => (Int,Int) -> Matrix a
zeros sz = constantMatrix sz 0

-- |Create an @nxm@ matrix of ones.
ones :: (Num a, Storable a) => (Int,Int) -> Matrix a
ones sz = constantMatrix sz 1

-- |Return the size of a matrix as a 2-tuple.
size :: Matrix a -> (Int,Int)
size x = (rows x, cols x)

-- |Concatenate matrices horizontally.
horzcat :: Element a => [Matrix a] -> Matrix a
horzcat = fromBlocks . return

-- |Concatenate matrices vertically.
vertcat :: Element a => [Matrix a] -> Matrix a
vertcat = fromBlocks . map return