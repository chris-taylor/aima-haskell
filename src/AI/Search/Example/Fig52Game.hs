{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AI.Search.Example.Fig52Game where

import AI.Search.Adversarial
import AI.Util.Util

----------------------------
-- Example Game (Fig 5.2) --
----------------------------

-- |Data type representing the example game.
data ExampleGame s a = ExampleGame deriving (Show)

-- |Instance of the example game.
exampleGame :: ExampleGame String Int
exampleGame = ExampleGame

-- |Definition of the example game in Fig 5.2 (mainly useful as an example of
--  how to create games).
instance Game ExampleGame String Int where
    initial g = "A"

    toMove g "A" = Max
    toMove g  _  = Min

    legalMoves _ s = case s `elem` ["A","B","C","D"] of
        True  -> [1,2,3]
        False -> []

    makeMove _ n "A" = ["B","C","D"] !! (n-1)
    makeMove _ n "B" = ["B1","B2","B3"] !! (n-1)
    makeMove _ n "C" = ["C1","C2","C3"] !! (n-1)
    makeMove _ n "D" = ["D1","D2","D3"] !! (n-1)

    utility _ s p = let u = util s in if p == Max then u else -u
        where
            util = listToFunction [ ("B1", 3), ("B2",12), ("B3", 8)
                                  , ("C1", 2), ("C2", 4), ("C3", 6)
                                  , ("D1",14), ("D2", 5), ("D3", 2) ]

    terminalTest t s = if s `elem` ["B1","B2","B3","C1","C2","C3","D1","D2","D3"]
        then True
        else False