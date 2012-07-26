module AI.Util.Matrix where

import Data.Array
import qualified Data.List as L

newtype Matrix a = M { unM :: Array (Int,Int) a }

instance Show a => Show (Matrix a) where
    show = concat . L.intersperse "\n" . showMat

showMat :: Show a => Matrix a -> [String]
showMat (M x) = [ unwords $ map show
                    [ x!(i,j) | i <- range (li,ui) ]
                |  j <- range(lj,uj) ]
    where
        ((li,lj),(ui,uj)) = bounds x

isScalar :: Matrix a -> Bool
isScalar (M x) = rangeSize (bounds x) == 1

instance Num a => Num (Matrix a) where
    (+) = addM
    (*) = mulM
    negate = M . fmap negate . unM
    signum = M . fmap signum . unM
    abs    = M . fmap abs . unM
    fromInteger n = M $ array ((1,1),(1,1)) [((1,1),fromInteger n)]

addM :: Num a => Matrix a -> Matrix a -> Matrix a
addM (M x) (M y) = M $ array resultBounds
                         [ (i, x!i + y!i) | i <- range resultBounds ]
        where resultBounds
                | bnds==bnds'   = bnds
                | otherwise     = error "addM: incompatible bounds"
              bnds  = bounds x
              bnds' = bounds y

mulM :: Num a => Matrix a -> Matrix a -> Matrix a
mulM x y
    | isScalar x = scalarMulM x y
    | isScalar y = scalarMulM y x
    | otherwise  = mulM' x y

scalarMulM :: Num a => Matrix a -> Matrix a -> Matrix a
scalarMulM (M s) (M x) = M $ fmap (* s!(1,1)) x

mulM' :: Num a => Matrix a -> Matrix a -> Matrix a
mulM' (M x) (M y) = M $ accumArray (+) 0 resultBounds
                            [((i,j), x!(i,k) * y!(k,j))
                                      | i <- range (li,ui),
                                        j <- range (lj',uj'),
                                        k <- range (lj,uj)  ]
        where ((li,lj),(ui,uj))         = bounds x
              ((li',lj'),(ui',uj'))     = bounds y
              resultBounds
                | (lj,uj)==(li',ui')    = ((li,lj'),(ui,uj'))
                | otherwise             = error "mulM: incompatible bounds"

mkArray :: Ix i => (i -> a) -> (i,i) -> Array i a
mkArray f bnds = array bnds [ (i, f i) | i <- range bnds ]

constArray :: (Int,Int) -> a -> Array (Int,Int) a
constArray bnds a = mkArray (const a) ((1,1),bnds)

eye :: Num a => Int -> Matrix a
eye n = M $ mkArray (\(i,j) -> if i == j then 1 else 0) ((1,1),(n,n))

ones :: Num a => (Int,Int) -> Matrix a
ones bnds = M $ constArray bnds 1

zeros :: Num a => (Int,Int) -> Matrix a
zeros bnds = M $ constArray bnds 0