module AI.Probability.Bayes where

import AI.Util.ProbDist

import qualified Data.Map as M

------------------
-- Drug Testing --
------------------

data Test = Pos | Neg deriving (Show,Eq)

data HeroinStatus = User | Clean deriving (Show,Eq)

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

---------------------------
-- Actually useful stuff --
---------------------------

bayes :: Dist a -> Dist a
bayes = normalize

condition :: Bool -> Dist ()
condition True  = return ()
condition False = D []