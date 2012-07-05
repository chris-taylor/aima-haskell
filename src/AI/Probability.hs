{-# LANGUAGE FlexibleInstances #-}

module AI.Probability where

import Control.Monad
import Data.Map (Map)
import GHC.Float

import qualified Data.Map as M

type Prob = Float

data Dist a = D { unD :: [(a,Prob)] } deriving (Show)

instance Functor Dist where
    fmap f (D xs) = D [ (f x,p) | (x,p) <- xs ]

instance Monad Dist where
    return x = D [(x,1)]

    (D xs) >>= f = D [ (y,p*q) | (x,p) <- xs, (y,q) <- unD (f x) ]

mapD :: (a -> b) -> Dist a -> Dist b
mapD = fmap

mapP :: (Prob -> Prob) -> Dist a -> Dist a
mapP f (D xs) = D (map (\(x,p) -> (x,f p)) xs)

filterP :: (a -> Bool) -> Dist a -> Dist a
filterP test (D xs) = normalize $ D $ filter (test . fst) xs

(|||) :: Dist a -> (a -> Bool) -> Dist a
p ||| condition = filterP condition p

vals :: Dist a -> [a]
vals (D xs) = map fst xs

probs :: Dist a -> [Prob]
probs (D xs) = map snd xs

normalize :: Dist a -> Dist a
normalize p = if total =~ 1.0
                then p
                else mapP (/total) p where total = sum (probs p)

isDist :: Dist a -> Bool
isDist p@(D xs) = firstAxiomHolds && secondAxiomHolds
    where
    firstAxiomHolds  = all (\x -> snd x >= 0) xs
    secondAxiomHolds = sum (probs p) =~ 1

collect :: (Ord a) => Dist a -> Dist a
collect (D xs) = D $ M.toList $ M.fromListWith (+) xs

-----------------------
-- Numeric Functions --
-----------------------

-- |Approximate floating point equality, with a tolerance of 1 part in 1000.
(=~) :: (Ord a, Fractional a) => a -> a -> Bool
x =~ y = abs (x/y - 1) < 0.001

----------------------
-- Convert to Float --
----------------------

-- |Type class for data which can be converted to a floating point number.
class ToFloat a where
    toFloat :: a -> Float

instance ToFloat Float where
    toFloat = id

instance ToFloat Double where
    toFloat = double2Float

instance ToFloat Int where
    toFloat = fromIntegral

instance ToFloat Integer where
    toFloat = fromIntegral

--------------------------------
-- Functions on Distributions --
--------------------------------

-- |Compute the expectation of a numeric distribution. The expectation is
--  defined to be
--
--  > sum (x_i * p_i) for i = 1 .. end
--
--  This is only defined for distributions over data that can be cast to Float.
expectation :: ToFloat a => Dist a -> Prob
expectation (D xs) = sum $ [ toFloat x * p | (x,p) <- xs ]

-------------------------------
-- Probability Distributions --
-------------------------------

-- |The Bernoulli distribution takes one of two possible values.
bernoulli :: Prob -> a -> a -> Dist a
bernoulli p a b = D [(a,p), (b,1-p)]

-- |A uniform distribution over a finite list assigns equal probability to each
--  of the elements of the list.
uniform :: [a] -> Dist a
uniform xs = D $ zip xs (repeat p) where p = 1 / fromIntegral (length xs)

d n = uniform [1..n]

die :: Dist Int
die = d 6