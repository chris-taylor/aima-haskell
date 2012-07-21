module AI.Util.Util where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified System.Random as R

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Error
import Data.Map (Map, (!))
import System.CPUTime
import System.Random
import System.Timeout

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

-- |Return the first element of a 3-tuple.
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- |Return the second element of a 3-tuple.
snd3:: (a,b,c) -> b
snd3 (_,b,_) = b

-- |Return the third element of a 3-tuple.
thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

--------------------
-- List Functions --
--------------------

-- |Return 'True' if the list is not null.
notNull :: [a] -> Bool
notNull = not . null

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

-- |Return a list of all (ordered) pairs of elements of a list.
orderedPairs :: [a] -> [(a,a)]
orderedPairs xs = [ (x,y) | x <- xs, y <- xs ]

-- |Return a list of all (unordered) pairs of elements from a list.
unorderedPairs :: [a] -> [(a,a)]
unorderedPairs []     = []
unorderedPairs (x:xs) = [ (x,y) | y <- xs ] ++ unorderedPairs xs

-- |Return 'True' if the first set is a subset of the second, i.e. if every
--  element of the first set is also an element of the second set.
isSubSet :: Eq a => [a] -> [a] -> Bool
xs `isSubSet` ys = all (`elem` ys) xs

-- |Given a list x :: [a], return a new list y :: [(Int,a)] which pairs every
--  element of the list with its position.
enumerate :: [a] -> [(Int,a)]
enumerate = zip [0..]

-- |Count the number of elements in a list that satisfy a predicate.
countIf :: (a -> Bool) -> [a] -> Int
countIf p xs = length (filter p xs)

-- |Return the element of a list that minimises a function. In case of a tie,
--  return the element closest to the front of the list.
argMin :: Ord b => [a] -> (a -> b) -> a
argMin xs f = L.minimumBy (O.comparing f) xs

-- |Return a list of all elements that minimise a given function.
argMinList :: Ord b => [a] -> (a -> b) -> [a]
argMinList xs f = map (xs!!) indices
    where
        ys      = map f xs
        minVal  = minimum ys
        indices = L.findIndices (== minVal) ys

-- |Return the element of a list that minimizes a function. In case of a tie,
--  choose randomly with the given generator.
argMinRandom :: (Ord b, RandomGen g) => g -> [a] -> (a -> b) -> (a, g)
argMinRandom g xs f = randomChoice g (argMinList xs f)

-- |Return the element of a list that minimizes a function. In case of a tie,
--  choose randomly.
argMinRandomIO :: Ord b => [a] -> (a -> b) -> IO a
argMinRandomIO xs f = getStdGen >>= \g -> return $ fst $ argMinRandom g xs f

-- |Return the element of the target list that maximises a function.
argMax :: (Ord b, Num b) => [a] -> (a -> b) -> a
argMax xs f = argMin xs (negate . f)

-- |Return a list of all elements that maximise a given function.
argMaxList :: (Ord b, Num b) => [a] -> (a -> b) -> [a]
argMaxList xs f = argMinList xs (negate . f)

-- |Return the element of a list that maximises a function. In case of a tie,
--  choose randomly with the given generator.
argMaxRandom :: (Ord b, Num b, RandomGen g) => g -> [a] -> (a -> b) -> (a, g)
argMaxRandom g xs f = argMinRandom g xs (negate . f)

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

-- |Return all lists of 'Bool' of length @n@. For example,
--
--  >>> bools 2
--  [[True,True],[True,False],[False,True],[False,False]] 
--
--  The returned list has length @2 ^ n@.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = do
    x  <- [True, False]
    xs <- bools (n-1)
    return (x:xs)

------------------
-- String Utils --
------------------

-- |Remove leading whitespace (spaces or tabs).
lstrip :: String -> String
lstrip = dropWhile (`elem` " \t")

-- |Remove trailing whitespace (spaces or tabs).
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- |Remove both leading and trailing whitespace (spaces or tabs).
strip :: String -> String
strip = rstrip . lstrip

-------------------
-- Map Functions --
-------------------

-- |A universal map maps all keys to the same value.
mkUniversalMap :: Ord k => [k] -> a -> Map k a
mkUniversalMap ks a = M.fromList $ zip ks (repeat a)

-------------------------
-- Monadic Combinators --
-------------------------

-- |Monadic 'when' statement.
whenM :: Monad m => m Bool -> m () -> m ()
whenM test s = test >>= \p -> when p s

-- |Monadic ternary 'if' statement.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM test a b = test >>= \p -> if p then a else b

-- |Run a REPL-style computation continuously.
untilM :: Monad m => (t -> Bool) -> m t -> (t -> m ()) -> m ()
untilM predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> untilM predicate prompt action

-- |Run a computation, ignoring the result (i.e. run it only for its side
--  effects).
ignoreResult :: Monad m => m a -> m ()
ignoreResult c = c >> return ()

-- |Ensure that a monadic computation doesn't throw any errors.
trapError :: MonadError e m => m () -> m ()
trapError c = c `catchError` \_ -> return ()

--------------------
-- Random Numbers -- 
--------------------

-- |Choose a random element from a list, given a generator.
randomChoice :: RandomGen g => g -> [a] -> (a, g)
randomChoice g [] = error "Empty list -- RANDOMCHOICE"
randomChoice g xs = (xs !! n, next)
    where
        (n, next) = randomR (0, length xs - 1) g

-- |Choose a random element from a list, in the IO monad.
randomChoiceIO :: [a] -> IO a
randomChoiceIO xs = getStdGen >>= \g -> return $ fst $ randomChoice g xs

-- |Given a random number generator, return 'True' with probability p.
probability :: (RandomGen g, Random a, Ord a, Num a) => g -> a -> (Bool, g)
probability g p = if p' < p then (True, g') else (False, g')
    where
        (p', g') = R.randomR (0,1) g

-- |Return @True@ with probability p.
probabilityIO :: (R.Random a, Ord a, Num a) => a -> IO Bool
probabilityIO p = getStdGen >>= \g -> return $ fst $ probability g p

--------------------
-- IO Combinators -- 
--------------------

-- |Read a line from stdin and return it.
readPrompt :: IO String
readPrompt = putStr "> " >> getLine

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
