module AI.Util.Util where

import qualified Data.List as L
import qualified Data.Ord as O
import qualified System.Random as R

import Control.Exception
import System.CPUTime

-----------------
-- Combinators --
-----------------

-- |The @|>@ combinator is left-to-right function composition with low
--  precedence. It is equivalent to @(|>) = flip ($)@.
(|>) :: a -> (a -> b) -> b
x |> f = f x 

-----------------------
-- Numeric Functions --
-----------------------

-- |Positive infinity.
posInf :: Fractional a => a
posInf = 1/0

-- |Negative infinity.
negInf :: Fractional a => a
negInf = -1/0

--------------------
-- List Functions --
--------------------

-- |Update the element at position i in a list.
insert :: Int -> a -> [a] -> [a]
insert 0 n (_:xs) = n : xs
insert i n (x:xs) = x : insert (i-1) n xs

-- |Given a list x :: [a], return a new list y :: [(Int,a)] which pairs every
--  element of the list with its position.
enumerate :: [a] -> [(Int,a)]
enumerate = zip [0..]

-- |Count the number of elements in a list that satisfy a predicate.
countIf :: (a -> Bool) -> [a] -> Int
countIf p xs = length (filter p xs)

-- |Return the element of the target list that maximises a function.
argMax :: (Ord b) => [a] -> (a -> b) -> a
argMax xs f = fst $ L.maximumBy (O.comparing snd) $ zip xs (map f xs)

-- |Return the element of a list that minimises a function.
argMin :: (Ord b) => [a] -> (a -> b) -> a
argMin xs f = fst $ L.minimumBy (O.comparing snd) $ zip xs (map f xs)

-- |Create a function from a list of (argument, value) pairs.
listToFunction :: (Eq a) => [(a,b)] -> a -> b
listToFunction xs x = case lookup x xs of
    Nothing -> error "Argument not found in list -- LISTTOFUNCTION"
    Just y  -> y

--------------------
-- Random Numbers -- 
--------------------

-- |Choose a random element from a list
randomChoiceIO :: [a] -> IO a
randomChoiceIO [] = error "Empty list -- RANDOMCHOICEIO"
randomChoiceIO xs = do
    n <- R.randomRIO (0,length xs - 1)
    return (xs !! n)

--------------------
-- IO Combinators -- 
--------------------

type Time = Double

timed :: a -> IO (a, Time)
timed x = do
    t1 <- getCPUTime
    r  <- evaluate x
    t2 <- getCPUTime
    let diff = fromIntegral (t2 - t1) / 10^12
    return (r, diff)

timeLimited :: Time -> [a] -> IO [a]
timeLimited remaining []     = return []
timeLimited remaining (x:xs) = if remaining < 0
    then return []
    else do
        (y,t) <- timed x
        ys    <- timeLimited (remaining - t) xs
        return (y:ys)

timeOut :: Time -> a -> IO (Maybe (a,t))
timeOut = undefined

timeLimited' :: Time -> [a] -> IO [a]
timeLimited' remaining []     = return []
timeLimited' remaining (x:xs) = do
    result <- timeOut remaining x
    case result of
        Nothing    -> return []
        Just (y,t) -> do
            ys <- timeLimited' (remaining - t) xs
            return (y:ys)


