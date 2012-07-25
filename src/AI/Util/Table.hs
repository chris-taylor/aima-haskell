{-# LANGUAGE ExistentialQuantification #-}

-- |This module contains routines for displaying and printing tables of data.
module AI.Util.Table where
  ( Showable(..)
  , printTable
  , showTable
  ) where

-- |A Showable is simply a box containing a value which is an instance of 'Show'.
data Showable = forall a. Show a => SB a

-- |To convert a 'Showable' to a 'String', just call 'show' on its contents.
instance Show Showable where
  show (SB x) = show x

-- |Print a table of data to stdout. You must supply the column width (number
--  of chars) and a list of row and column names.
printTable :: Int           -- ^ Column width
           -> [[Showable]]  -- ^ Data
           -> [String]      -- ^ Column names (including the 0th column)
           -> [String]      -- ^ Row names
           -> IO ()
printTable pad xs header rownames =
    mapM_ putStrLn (showTable pad xs header rownames)

-- |Return a table as a list of strings, one row per line. This routine is
--  called by 'printTable'
showTable :: Int            -- ^ Column width
          -> [[Showable]]   -- ^ Data
          -> [String]       -- ^ Column names
          -> [String]       -- ^ Row names
          -> [String]
showTable pad xs header rownames =
    let dashes = replicate (length header) (replicate pad '-')
        hzline = showRow pad "+" dashes
        hdline = showRow pad "|" header
        rows'  = zipWith (:) rownames (map (map show) xs)
        rows   = map (showRow pad "|") rows'
    in [hzline,hdline,hzline] ++ rows ++ [hzline]

-- |Convert a single row of a table to a string, padding each cell so that 
--  it is of uniform width.
showRow :: Int -> String -> [String] -> String
showRow pad sep xs = sep ++ (concatMap showCell cells)
    where
        trim pad str = let n = length str
                           m = max 0 (pad - n)
                        in take pad str ++ replicate m ' '
        showCell cel = cel ++ sep
        cells        = map (trim pad) xs

