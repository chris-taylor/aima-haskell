module AI.Util.Util where

import qualified Data.List as L
import qualified Data.Ord as O
import qualified System.Random as R

import Control.Concurrent.STM
import Control.DeepSeq
import System.CPUTime
import System.Timeout

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

-- |Compute a pure value and return it along with the number of microseconds
--  taken for the computation.
timed :: (NFData a) => a -> IO (a, Int)
timed x = do
    t1 <- getCPUTime
    r  <- return $!! x
    t2 <- getCPUTime
    let diff = fromIntegral (t2 - t1) `div` 1000000
    return (r, diff)

-- |Given a time limit (in microseconds) and a list, compute as many elements
--  of the list as possible within the time limit.
timeLimited :: (NFData a) => Int -> [a] -> IO [a]
timeLimited t xs = do
    v <- newTVarIO []
    timeout t (forceIntoTVar v xs)
    readTVarIO v

-- |Compute the elements of a list one by one, consing them onto the front
--  of a @TVar@ as they are computed. Note that the result list will be
--  in reverse order.
forceIntoTVar :: (NFData a) => TVar [a] -> [a] -> IO ()
forceIntoTVar v xs = mapM_ (forceCons v) xs

-- |Force a pure value, and cons it onto the front of a list stored in a @TVar@.
forceCons :: (NFData a) => TVar [a] -> a -> IO ()
forceCons v x = x `deepseq` atomically $ modifyTVar2 v (x:)

-- |Modify the value of a transactional variable
modifyTVar2 :: TVar a -> (a -> a) -> STM ()
modifyTVar2 v f = readTVar v >>= writeTVar v . f
