{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AI.Search.Example.TicTacToe where

import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.List as L

import AI.Search.Adversarial
import AI.Util.Util

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

    heuristic _ = heuristicTTT [1,-1]

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

--------------------------
-- Displaying the Board --
--------------------------

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

------------------------
-- Compute Heuristics --
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

-- |Return @True@ if a cell is a threat cell.
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

----------
-- Demo --
----------

-- |Play a game of tic-tac-toe against a player using the minimax algorithm
--  with full search. This player is impossible to beat - the best you can
--  do is to draw.
demo :: IO ()
demo = do playGameIO ticTacToe queryPlayer minimaxFullSearchPlayer
          return ()