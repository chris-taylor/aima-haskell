module AI.Probability.Bayes where

import AI.Util.ProbDist
import AI.Util.Util

import Data.Map (Map, (!))
import qualified Data.Map as M

------------------
-- Drug Testing --
------------------

data Test = Pos | Neg deriving (Show,Eq,Ord)

data HeroinStatus = User | Clean deriving (Show,Eq,Ord)

drugTest1 :: Dist (HeroinStatus,Test)
drugTest1 = do
    heroinStatus <- percentUser 0.1
    testResult <- if heroinStatus == User
        then percentPos 99
        else percentPos 1
    return (heroinStatus, testResult)

percentUser p = bernoulli (p/100) User Clean
percentPos  p = bernoulli (p/100) Pos Neg

drugTest2 :: Dist (Maybe HeroinStatus)
drugTest2 = do
    (heroinStatus,testResult) <- drugTest1
    return (if testResult == Pos
        then Just heroinStatus
        else Nothing)

drugTest3 :: Dist HeroinStatus -> Dist HeroinStatus
drugTest3 prior = bayes $ do
    heroinStatus <- prior
    testResult <- if heroinStatus == User
        then percentPos 99
        else percentPos 1
    condition (testResult == Pos)
    return heroinStatus

----------
-- Spam --
----------

data MsgType = Spam | Ham deriving (Show,Eq,Ord,Enum,Bounded)

hasWord :: String -> Dist MsgType -> Dist MsgType
hasWord word prior = do
    msgType <- prior
    wordPresent <- wordPresentDist 1 msgType word
    condition wordPresent
    return msgType

hasWords :: [String] -> Dist MsgType -> Dist MsgType
hasWords []     prior = prior
hasWords (w:ws) prior = hasWord w (hasWords ws prior)

-- List lookup
entryFor :: Enum a => a -> [b] -> b
entryFor a bs = bs !! fromEnum a

-- [Spam count, ham count]
msgCounts :: [Int]
msgCounts = [102, 57]

-- Number of spams and hams containing each word
wordCountTable :: M.Map String [Int]
wordCountTable = M.fromList
    [ ("free",  [57, 6])
    , ("bayes", [1, 10])
    , ("monad", [0, 22]) ]

-- Prior distribution
msgTypePrior :: Dist MsgType
msgTypePrior = weighted $ zip [Spam,Ham] msgCounts

wordPresentDist :: Int -> MsgType -> String -> Dist Bool
wordPresentDist k msgType word = boolDist (n / total)
    where
        wordCounts = findWordCounts word
        n     = fromIntegral $ entryFor msgType (laplaceSmooth k wordCounts)
        total = fromIntegral $ entryFor msgType (laplaceSmooth k msgCounts)

boolDist :: Prob -> Dist Bool
boolDist p = bernoulli p True False

findWordCounts :: String -> [Int]
findWordCounts word = M.findWithDefault [0,0] word wordCountTable

-------------------------------------
-- Improvements to spam classifier --
-------------------------------------

uniformAll :: (Enum a, Bounded a) => Dist a
uniformAll = uniform allValues

allValues :: (Enum a, Bounded a) => [a]
allValues = enumFromTo minBound maxBound

characteristic :: (Enum a, Bounded a) => (Dist a -> b) -> b
characteristic f = f uniformAll

score f = distance (characteristic f) uniformAll

distance :: Ord a => Dist a -> Dist a -> Float
distance d1 d2 = sum $ map (^2) (zipWith (-) ps1 ps2)
    where
        ps1 = probs $ collect $ bayes d1
        ps2 = probs $ collect $ bayes d2

laplaceSmooth :: Int -> [Int] -> [Int]
laplaceSmooth k = map (+k)

--vectorFromDist = map doubleFromProb (probsFrom)

----------------
-- Monty Hall --
----------------

data Result = Win | Lose deriving (Eq,Show,Ord)
data Choice = Switch | Stay


doors = [1..3]

montyHall :: Int -> Choice -> Dist Result
montyHall yourDoor strategy = collect $ do
    carDoor      <- uniform doors
    montysChoice <- monty carDoor yourDoor
    let otherDoor = head $ filter (/=yourDoor) $ filter (/=montysChoice) doors
    return (case strategy of
        Stay   -> if  yourDoor == carDoor then Win else Lose
        Switch -> if otherDoor == carDoor then Win else Lose)

monty carDoor yourDoor = uniform $ filter (/=carDoor) $ filter (/=yourDoor) doors

--------------
-- Boy/Girl --
--------------

data Child = Boy | Girl deriving (Show,Eq,Ord)

child = uniform [Boy,Girl]

test2 = bayes $ do
    c1 <- child
    c2 <- child
    condition (c1 == Boy || c2 == Boy)
    return c1

-----------------
-- Biased Coin --
-----------------

data Coin = Biased | Unbiased deriving (Eq,Ord,Show)
data Toss = Head | Tail deriving (Eq,Ord,Show)

coin = weighted [(Biased,1),(Unbiased,999)]

toss Biased   = certainly Head
toss Unbiased = uniform [Head,Tail]

test3 = bayes $ do
    c <- coin
    result <- sequence (replicate 10 $ toss c)
    condition (all (==Head) result)
    next <- toss c
    return next

-------------
-- Raining --
-------------

rain [] = boolDist 0.2

sprinkler [rain] = if rain then boolDist 0.01 else boolDist 0.4

grass [sprinkler,rain] = case [sprinkler,rain] of
    [False,False] -> certainly False
    [False,True]  -> boolDist 0.8
    [True,False]  -> boolDist 0.9
    [True,True]   -> boolDist 0.99

isGrassWet = do
    r <- rain []
    s <- sprinkler [r]
    g <- grass [s,r]
    return g

--didItRain grassWet = bayes $ do
--    r <- rain
--    s <- sprinkler r
--    g <- grass (s,r)
--    condition (g == grassWet)
--    return r

---------------
-- Bayes Net --
---------------

data Node e = Node { nodeParents :: [e], nodeDist :: [Bool] -> Dist Bool }
    --deriving (Eq,Ord)

newtype BayesNet e = BayesNet (Map e (Node e))
    --deriving (Eq,Ord)

fromList :: Ord e => [ (e, [e], [Prob]) ] -> BayesNet e
fromList = BayesNet . (foldl doOne M.empty)
    where
        doOne m (ev,cond,ps) = M.insert ev (Node cond (mkFun cond ps)) m

mkFun :: [e] -> [Prob] -> [Bool] -> Dist Bool
mkFun cond ps = if length ps /= 2 ^ length cond
    then error "Invalid length for probability table"
    else listToFunction $ zip (bools $ length cond) $ map boolDist ps

net :: BayesNet Char
net = fromList [ ('R', "",   [0.2])
               , ('S', "R",  [0.01, 0.4])
               , ('G', "SR", [0.99, 0.9, 0.8, 0]) ]

---------------------------
-- Actually useful stuff --
---------------------------

bayes :: Ord a => Dist a -> Dist a
bayes = collect . normalize

condition :: Bool -> Dist ()
condition True  = return ()
condition False = D []