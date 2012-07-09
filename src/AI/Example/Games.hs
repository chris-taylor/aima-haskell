{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AI.Example.Games where

import Control.Exception
import Data.Map (Map, (!))
import Data.Maybe (catMaybes)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified System.Random as R

import AI.Games
import AI.Util.Util
import AI.Util.Graph (Graph)

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

----------------------------------
-- Tic Tac Toe on a H x V board --
----------------------------------

-- |Data type for K-in-a-row  tic tac toe, on a H x V board.
data TicTacToe s a = TTT { hT :: Int, vT :: Int, kT :: Int } deriving (Show)

-- |A move in tic tac toe is a pair of integers indicating the row and column,
--  indexed from zero.
type TTMove = (Int,Int)

-- |Each counter in tic-tac-toe is either an @O@ or an @X@.
data TTCounter = O | X deriving (Eq,Show)

-- |A tic tac toe board is a map from board positions to counters. Note that
--  @M.lookup (x,y) board@ will return @Nothing@ if square @(x,y)@ is empty. 
type TTBoard = Map TTMove TTCounter

-- |The state of a tic tac toe game is defined by the board. We also store the
--  player whose move is next, the utility of this state (which is only nonzero
--  if the state is terminal) and the size of the board, for convenience.
data TTState = TTS
    { boardTT :: TTBoard
    , toMoveTT :: TTCounter
    , utilityTT :: Utility
    , limsTT :: (Int,Int,Int) }

-- |This 'Game' instance defines the rules of tic tac toe. Note that whenever
--  a move is made, we compute the utility of the newly created state on the
--  fly. This avoids having to write an expensive function to decide if any
--  player has won for a specific board state. The game is over when either
--  a player has one, or there are no legal moves left to make.
instance Game TicTacToe TTState TTMove where
    initial (TTT h v k) = TTS M.empty O 0 (h,v,k)

    toMove _ s = if toMoveTT s == O then Max else Min

    legalMoves (TTT h v _) (TTS board _ _ _) =
        [ (i,j) | i <- [0..h-1], j <- [0..v-1], M.notMember (i,j) board ]

    makeMove g move s@(TTS board p _ n) =
        let u = computeUtility s move
        in TTS (M.insert move p board) (other p) u n

    utility _ s p = let u = utilityTT s in if p == Max then u else -u

    terminalTest g s = utilityTT s /= 0 || null (legalMoves g s)

    heuristic _ = heuristicTTT [1.0,-1.0,0,0]

-- |A 3x3 instance of tic tac toe.
ticTacToe :: TicTacToe TTState TTMove
ticTacToe = TTT 3 3 3

-- |A useful function that interchanges @O@s and @X@s.
other :: TTCounter -> TTCounter
other O = X
other X = O

-- |In our game, @Max@ always plays the @O@ counter and @Min@ plays @X@.
counter :: Player -> TTCounter
counter Max = O
counter Min = X

-- |Helper function that computes the utility of a state after a particular
--  move is played.
computeUtility :: TTState -> TTMove -> Utility
computeUtility s@(TTS _ player _ _) move = if kInARow s move player
    then if player == O then posInf else negInf
    else 0

-- |Given the current state of the board, return @True@ if putting a counter
--  into a specific square would win the game.
kInARow :: TTState -> TTMove -> TTCounter -> Bool
kInARow state move player = f (1,0) || f (0,1) || f (1,1) || f (1,-1)
    where
        f = kInARow' state move player

-- |A helper function for 'kInARow'. Given a state of the board, does adding a
--  counter to a specific square given k-in-a-row in the specified direction?
kInARow' :: TTState -> TTMove -> TTCounter -> (Int,Int) -> Bool
kInARow' (TTS board _ _ (_,_,k)) (x,y) p (dx,dy) = n1 + n2 - 1 >= k
    where
        board' = M.insert (x,y) p board
        fw = map (`M.lookup` board') ( zip [x,x+dx..] [y,y+dy..] )
        bk = map (`M.lookup` board') ( zip [x,x-dx..] [y,y-dy..] )
        n1 = length $ takeWhile (== Just p) fw
        n2 = length $ takeWhile (== Just p) bk

-- |The Show instance for 'TTState' creates a human-readable representation of
--  the board.
instance Show TTState where
    show s = concat $ concat $ L.intersperse [row] $
                map ((++["\n"]) . L.intersperse "|") (toChars s)
        where
            (h,_,_) = limsTT s
            row = (concat $ replicate (h-1) "---+") ++ "---\n"

-- |A helper function for @Show TTState@ that converts each position on the
--  board to its @Char@ representation.
toChars :: TTState -> [[String]]
toChars (TTS board _ _ (h,v,_)) = reverse $ map (map f) board'
    where
        board' = [ [ M.lookup (i,j) board | i <- [0..h-1] ] | j <- [0..v-1] ]
        f (Just O) = " O "
        f (Just X) = " X "
        f Nothing  = "   "

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

    sortMoves (C (TTT h _ _)) as = L.sortBy (O.comparing f) as
        where f x = abs (x - h `div` 2)

    legalMoves (C g) s@(TTS board _ _ _) =
        [ x+1 | (x,y) <- legalMoves g s, y == 0 || (x,y-1) `M.member` board ]

    heuristic g = heuristicTTT [0.1,-0.1,0.9,-0.9]

-- |Return the lowest row in the specified column which is currently unoccupied.
lowestUnoccupied :: Int -> TTState -> Int
lowestUnoccupied col (TTS board _ _ (_,v,_)) = 
    let coords   = map (\row -> (col,row)) [0..v-1]
        counters = map (`M.lookup` board) coords
    in (countIf (/=Nothing) counters)

------------------------
-- Compute heuristics --
------------------------

-- |A heuristic function for Tic Tac Toe. The value is a weighted
--  combination of simpler heuristic functions.
heuristicTTT :: [Double] -> TTState -> Player -> Utility
heuristicTTT weights s p = sum $ zipWith (*) weights [n1,n2,n3,n4]
    where
        n1 = fromIntegral (numWinningLines p s)
        n2 = fromIntegral (numWinningLines (opponent p) s)
        n3 = fromIntegral (numThreats p s)
        n4 = fromIntegral (numThreats (opponent p) s)

-- |Compute the number of winning lines heuristic for a particular player. A
--  winning line is defined to be a line of k squares, which contains at least
--  one of your counters, and none of the opponents counters.
numWinningLines :: Player -> TTState -> Int
numWinningLines p s = length $ filter (isWinningLine $ counter p) (allLines s)

-- |Compute the number of threats heuristic for a particular player. A threat
--  cell is a cell that would win the game if it was filled in, but is not
--  blockable on the next move (i.e. it is not on the bottom row and does not
--  have a counter directly beneath it).
numThreats :: Player -> TTState -> Int
numThreats p s@(TTS _ _ _ (h,v,_)) = length $ filter (isThreat s p) xs
    where
        xs = [ (i,j) | i <- [0..h-1], j <- [0..v-1] ]

-- |Return @True@ is a specic cell is a threat cell.
isThreat :: TTState -> Player -> (Int,Int) -> Bool
isThreat s@(TTS board _ _ _) p (x,y) =
    y /= 0 && (x,y-1) `M.notMember` board && kInARow s (x,y) (counter p)

-- |Return @True@ if a line of pieces is a winning line.
isWinningLine :: TTCounter -> [Maybe TTCounter] -> Bool
isWinningLine c xs = c `elem` ys && not (other c `elem` ys)
    where
        ys = catMaybes xs

-- |Return a list of all of the lines on the board.
allLines :: TTState -> [[Maybe TTCounter]]
allLines s = concat [ linesInDir s (1,0), linesInDir s (0,1)
                    , linesInDir s (1,1), linesInDir s (1,-1) ]

-- |Return all of the lines on the board in the specified direction.
linesInDir :: TTState -> (Int,Int) -> [[Maybe TTCounter]]
linesInDir s@(TTS board _ _ (h,v,k)) dir = 
    map (\p -> lineThrough s p dir) pts
    where
        pts = case dir of
            (1,0)  -> [ (x,y) | x <- [0..h-k], y <- [0..v-1] ]
            (0,1)  -> [ (x,y) | x <- [0..h-1], y <- [0..v-k] ]
            (1,1)  -> [ (x,y) | x <- [0..h-k], y <- [0..v-k] ]
            (1,-1) -> [ (x,y) | x <- [0..h-k], y <- [k-1..v-1] ]

-- |Return the line starting in cell (x,y) and continuing in direction (dx,dy)
lineThrough :: TTState -> (Int,Int) -> (Int,Int) -> [Maybe TTCounter]
lineThrough (TTS board _ _ (h,v,k)) (x,y) (dx,dy) = 
    take k $ map (`M.lookup` board) ( zip [x,x+dx..] [y,y+dy..] )