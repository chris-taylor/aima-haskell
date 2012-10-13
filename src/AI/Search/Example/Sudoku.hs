{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AI.Search.Example.Sudoku where

import Control.Monad
import Data.Map (Map, (!))

import qualified Data.List as L
import qualified Data.Map as M

import AI.Search.CSP
import AI.Util.Util

------------
-- Sudoku --
------------

data Sudoku v a = Sudoku (Domain String Char) deriving Show

instance CSP Sudoku String Char where
    vars s = squares
    domains (Sudoku dom) = dom
    neighbours s = M.fromList peers
    constraints s x xv y yv = (xv /= yv) || not (x `elem` neighbours s ! y)
        
        

cross :: [a] -> [a] -> [[a]]
cross xs ys = [ [x,y] | x <- xs, y <- ys ]

digits   = "123456789"
rows     = "abcdefghi"
cols     = digits
squares  = cross rows cols
unitlist = [ cross rows c | c <- map return cols ] ++
           [ cross r cols | r <- map return rows ] ++
           [ cross rs cs | rs <- ["abc","def","ghi"], cs <- ["123","456","789"] ]
units    = [ (s, [ u | u <- unitlist, s `elem` u ]) | s <- squares ]
peers    = [ (s, L.delete s $ L.nub $ concat u) | (s,u) <- units ]

parseGrid :: String -> Sudoku String Char
parseGrid grid =
    Sudoku $ foldr update (mkUniversalMap squares digits) initial
    where
        update (x,y) = if y `elem` digits
            then M.insert x [y]
            else M.insert x digits
        initial = zip squares $ filter (`elem` ( "0." ++ digits)) grid

------------------------------------------
-- Example Sudokus (from Project Euler) --
------------------------------------------

sudoku1 = parseGrid "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
sudoku2 = parseGrid "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
sudoku3 = parseGrid "000000907000420180000705026100904000050000040000507009920108000034059000507000000"

-----------
-- Demos --
-----------

demo :: IO ()
demo = do
    let sudokus = [sudoku1,sudoku2,sudoku3]
    forM_ sudokus $ \s -> case backtrackingSearch s fastOpts of
        Nothing  -> putStrLn "No solution found."
        Just sol -> do putStrLn "Solution found:"
                       print sol
