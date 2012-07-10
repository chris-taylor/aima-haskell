module AI.Util.Util where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified System.Random as R

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Map (Map, (!))
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

---------------------
-- Maybe Functions --
---------------------

-- |Return 'True' if a 'Maybe' value is 'Nothing', else 'False'.
no :: Maybe a -> Bool
no Nothing = True
no _       = False

---------------------
-- Tuple Functions --
---------------------

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3:: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

--------------------
-- List Functions --
--------------------

-- |Update the element at position i in a list.
insert :: Int -> a -> [a] -> [a]
insert 0 n (_:xs) = n : xs
insert i n (x:xs) = x : insert (i-1) n xs

-- |Delete every occurence of this element from the list
deleteEvery :: Eq a => a -> [a] -> [a]
deleteEvery x []     = []
deleteEvery x (y:ys) = if y == x then deleteEvery x ys else y : deleteEvery x ys

-- |Delete all the elements of the first list from the second list
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll xs []     = []
deleteAll xs (y:ys) = if y `elem` xs then deleteAll xs ys else y : deleteAll xs ys

-- |Given a list x :: [a], return a new list y :: [(Int,a)] which pairs every
--  element of the list with its position.
enumerate :: [a] -> [(Int,a)]
enumerate = zip [0..]

-- |Count the number of elements in a list that satisfy a predicate.
countIf :: (a -> Bool) -> [a] -> Int
countIf p xs = length (filter p xs)

-- |Return the element of a list that minimises a function. In case of a tie,
--  return the element closest to the front of the list.
argMin :: (Ord b) => [a] -> (a -> b) -> a
argMin xs f = fst $ L.minimumBy (O.comparing snd) $ zip xs (map f xs)

-- |Return a list of all elements that minimise a given function.
argMinList :: (Ord b) => [a] -> (a -> b) -> [a]
argMinList xs f = map (xs!!) indices
    where
        ys      = map f xs
        minVal  = minimum ys
        indices = L.findIndices (== minVal) ys

-- |Return the element of a list that minimizes a function. In case of a tie,
--  choose randomly.
argMinRandomIO :: (Ord b) => [a] -> (a -> b) -> IO a
argMinRandomIO xs f = randomChoiceIO (argMinList xs f)

-- |Return the element of the target list that maximises a function.
argMax :: (Ord b, Num b) => [a] -> (a -> b) -> a
argMax xs f = argMin xs (negate . f)

-- |Return a list of all elements that maximise a given function.
argMaxList :: (Ord b, Num b) => [a] -> (a -> b) -> [a]
argMaxList xs f = argMinList xs (negate . f)

-- |Return the element of a list that maximises a function. In case of a tie,
--  choose randomly.
argMaxRandomIO :: (Ord b, Num b) => [a] -> (a -> b) -> IO a
argMaxRandomIO xs f = argMinRandomIO xs (negate . f)

-- |Create a function from a list of (argument, value) pairs.
listToFunction :: (Ord a) => [(a,b)] -> a -> b
listToFunction xs = (M.fromList xs !)

-- |Transpose a list of lists.
transpose :: [[a]] -> [[a]]
transpose xs = if or (map null xs)
    then []
    else let heads = map head xs
             tails = map tail xs
          in heads : transpose tails

-------------------
-- Map Functions --
-------------------

-- |A universal map maps all keys to the same value.
mkUniversalMap :: Ord k => [k] -> a -> Map k a
mkUniversalMap ks a = M.fromList $ zip ks (repeat a)

--------------------
-- Random Numbers -- 
--------------------

-- |Choose a random element from a list
randomChoiceIO :: [a] -> IO a
randomChoiceIO [] = error "Empty list -- RANDOMCHOICEIO"
randomChoiceIO xs = do
    n <- R.randomRIO (0,length xs - 1)
    return (xs !! n)

-- |Return @True@ with probability p.
probabilityIO :: (R.Random a, Ord a, Num a) => a -> IO Bool
probabilityIO p = do
    p' <- R.randomRIO (0,1)
    return (if p' < p then True else False)

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
