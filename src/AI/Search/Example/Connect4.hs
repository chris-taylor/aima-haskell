{-# LANGUAGE FlexibleInstances #-}

module AI.Search.Example.Connect4 where

import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.List as L

import AI.Search.Adversarial
import AI.Search.Example.TicTacToe
import AI.Util.Util

---------------
-- Connect 4 --
---------------

-- |The 'Connect4' data type is a wrapper around 'TicTacToe', which allows us
--  to inherit most of its behaviour.
data Connect4 s a = C (TicTacToe TTState TTMove)

-- |Type for Connect 4 state.
type C4State = TTState

-- |Type for Connect 4 moves.
type C4Move = Int

-- |The standard Connect 4 board.
connect4 :: Connect4 C4State C4Move
connect4 = C (TTT 7 6 4)

-- |A Connect 4 game is identical to tic tac toe in most respects. It differs in
--  the set of legal moves, the fact that it sorts moves so that those closest
--  to the center are considered first, and the heuristic function.
instance Game Connect4 C4State C4Move where
    initial      (C g)        = initial g
    toMove       (C g) s      = toMove g s
    utility      (C g) s p    = utility g s p
    terminalTest (C g) s      = terminalTest g s

    makeMove     (C g) col s  = let row = lowestUnoccupied (col-1) s
                                in makeMove g (col-1, row) s

    sortMoves (C (TTT h _ _)) as = L.sortBy (comparing f) as
        where f x = abs (x - (h+1) `div` 2)

    legalMoves (C g) s@(TTS board _ _ _) =
        [ x+1 | (x,y) <- legalMoves g s, y == 0 || (x,y-1) `M.member` board ]

    heuristic g = heuristicTTT [0.1,-0.1,0.9,-0.9]

-- |Return the lowest row in the specified column which is currently unoccupied.
lowestUnoccupied :: Int -> TTState -> Int
lowestUnoccupied col (TTS board _ _ (_,v,_)) = 
    let coords   = map (\row -> (col,row)) [0..v-1]
        counters = map (`M.lookup` board) coords
    in (countIf (/=Nothing) counters)