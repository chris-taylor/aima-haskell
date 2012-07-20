{-# LANGUAGE FlexibleInstances #-}

module AI.Util.ProbDist where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.Map (Map)
import GHC.Float

import qualified Data.List as L
import qualified Data.Map as M

type Prob = Float

data Dist a = D { unD :: [(a,Prob)] }

instance Functor Dist where
    fmap f (D xs) = D [ (f x,p) | (x,p) <- xs ]

instance Applicative Dist where
    pure x = D [(x,1)]
    (D fs) <*> (D xs) = D $ [ (f x,p*q) | (f,q) <- fs, (x,p) <- xs ]

instance Monad Dist where
    return x = D [(x,1)]
    (D xs) >>= f = D [ (y,p*q) | (x,p) <- xs, (y,q) <- unD (f x) ]

-- |Map over the values of a probability distribution.
mapD :: (a -> b) -> Dist a -> Dist b
mapD = fmap

-- |Map over the probabilities of a probability distribution.
mapP :: (Prob -> Prob) -> Dist a -> Dist a
mapP f (D xs) = D (map (\(x,p) -> (x,f p)) xs)

-- |Filter the values of a probability distribution according to some predicate.
filterD :: (a -> Bool) -> Dist a -> Dist a
filterD test (D xs) = D $ filter (test . fst) xs

-- |Return the distribution that results from conditioning on some predicate.
--  Note that if the condition is not satisfied by any member of the initial
--  distribution, this procedure will give nonsensical results.
(|||) :: Dist a -> (a -> Bool) -> Dist a
p ||| condition = normalize (filterD condition p)

-- |Return the probability that the predicate is satisfied by a distribution.
(??) :: (a -> Bool) -> Dist a -> Prob
test ?? dist = sum . probs $ filterD test dist

-- |Return a list of all the values in a distribution.
vals :: Dist a -> [a]
vals (D xs) = map fst xs

-- |Return a list of all the probabilities in a distribution.
probs :: Dist a -> [Prob]
probs (D xs) = map snd xs

-- |Check if a given @Dist@ values satisfies the conditions required to be a
--  probability distribution, i.e. the probability associated with each element
--  is positive, and the probabilities sum to 1.
isDist :: Dist a -> Bool
isDist p@(D xs) = firstAxiomHolds && secondAxiomHolds
    where
    firstAxiomHolds  = all (\x -> snd x >= 0) xs
    secondAxiomHolds = sum (probs p) =~ 1

-- |Normalize a probability distribution. It may be necessary to call this after
--  filtering some values from a distribution.
normalize :: Dist a -> Dist a
normalize p = if total =~ 1.0
                then p
                else mapP (/total) p where total = sum (probs p)

-- |Collect equal values in a probability distribution. Since we cannot restrict
--  the values of a probability distribution to those with an @Eq@ instance, it
--  may sometimes be necessary to call this function to avoid explosive growth
--  in the number of elements in a distribution.
collect :: (Ord a) => Dist a -> Dist a
collect (D xs) = D $ M.toList $ M.fromListWith (+) xs

-- |Apply Bayes rule to a distribution. This is really just a convenience
--  function. It normalizes the distribution, and collects results. Typically
--  you would use it immediately after a @do@ block that uses the 'condition'
--  function to filter out unnnecessary values.
bayes :: Ord a => Dist a -> Dist a
bayes = collect . normalize

-- |Condition over an event in a do block. This can be used to filter out
--  unwanted results from a distribution. Note that if any filtering is done,
--  the distribution returned will be unnormalized. This is equivalent to
--  'guard' - in fact we could just make 'Dist' into an instance of 'MonadPlus'.
condition :: Bool -> Dist ()
condition True  = return ()
condition False = D []

-------------------
-- Show Instance --
-------------------

instance Show a => Show (Dist a) where
    show (D xs) = concat $ L.intersperse "\n" $ map disp xs
        where
            disp (x,p) = show x ++ replicate (pad x) ' ' ++ showProb p
            pad x      = n - length (show x) + 2
            n          = maximum $ map (length . show . fst) xs

showProb :: Prob -> String
showProb p = show intPart ++ "." ++ show fracPart ++ "%"
    where
        digits   = round (1000 * p)
        intPart  = digits `div` 10
        fracPart = digits `mod` 10

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

-- |Compute the entropy of a distribution, returning the result in /nats/.
--  Note that it is necessary to collect like results first, to ensure that
--  the true entropy is calculated.
entropy :: Ord a => Dist a -> Prob
entropy (D xs) = negate $ sum [ if p /= 0 then p * log p else 0 | (_,p) <- xs ]

-- |Compute the entropy of a distribution, returning the result in /bits/.
--  Note that it is necessary to collect like results first, to ensure that
--  the true entropy is calculated.
entropyBits :: Ord a => Dist a -> Prob
entropyBits d = entropy d / log 2

-------------------------------
-- Probability Distributions --
-------------------------------

-- |A trivial probability distribution that always takes the same value.
certainly :: a -> Dist a
certainly = return

-- |The Bernoulli distribution takes one of two possible values.
bernoulli :: Prob -> a -> a -> Dist a
bernoulli p a b = D [(a,p), (b,1-p)]

-- |A uniform distribution over a finite list assigns equal probability to each
--  of the elements of the list.
uniform :: [a] -> Dist a
uniform xs = D $ zip xs (repeat p) where p = 1 / fromIntegral (length xs)

-- |A weighted distribution over a finite list. The weights give the relative
--  probabilities attached to each outcome.
weighted :: [(a,Int)] -> Dist a
weighted lst = D $ zip xs ps
    where
        (xs,ws) = unzip lst
        ps      = map (\w -> fromIntegral w / fromIntegral (sum ws)) ws

-- |Return the empirical distribution over a list, i.e. choose each element
--  in proportion to how many times it appears in the list.
empirical :: Ord a => [a] -> Dist a
empirical xs = weighted $ M.toList $ M.fromListWith (+) $ zip xs [1..]

-- |Select @n@ elements from a list without replacement.
select :: Eq a => Int -> [a] -> Dist [a]
select n = mapD (reverse . fst) . selectMany n

-- |Select @n@ elements from a list uniformly at random without replacement,
--  also returning the list of remaining elements.
selectMany :: Eq a => Int -> [a] -> Dist ([a],[a])
selectMany 0 xs = return ([],xs)
selectMany n xs = do
    (v, xs1) <- selectOne xs
    (vs,xs2) <- selectMany (n-1) xs1
    return (v:vs,xs2)

-- |Select a single element from a list uniformly at random, also returning
--  the list that remains.
selectOne :: Eq a => [a] -> Dist (a,[a])
selectOne xs = uniform [(x, L.delete x xs) | x <- xs ]

--------------------------------
-- Functions on Distributions --
--------------------------------

joinWith :: Ord c => (a -> b -> c) -> Dist a -> Dist b -> Dist c
joinWith f (D xs) (D ys) =
    collect $ D [ (f x y, p*q) | (x,p) <- xs, (y,q) <- ys ]

addD :: (Num a, Ord a) => Dist a -> Dist a -> Dist a
addD = joinWith (+)

subD :: (Num a, Ord a) => Dist a -> Dist a -> Dist a
subD = joinWith (-)

mulD :: (Num a, Ord a) => Dist a -> Dist a -> Dist a
mulD = joinWith (*)

sumD :: (Num a, Ord a) => [Dist a] -> Dist a
sumD = L.foldl' addD (return 0)

prodD :: (Num a, Ord a) => [Dist a] -> Dist a
prodD = L.foldl' mulD (return 1)

--------------
-- Sampling --
--------------

-- |Create a random sampler from a probability distribution.
sample :: MonadRandom m => Dist a -> m a
sample (D []) = error "AI.Util.ProbDist.sample called with empty distribution"
sample (D xs) = do
    v <- getRandomR (0,1)
    return $ fst . head . dropWhile (\(x,p) -> p < v) $ cumulative
    where
        cumulative = scanl1 (\(x,p) (y,q) -> (y,p+q)) xs
