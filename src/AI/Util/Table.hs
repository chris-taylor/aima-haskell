module AI.Util.Table (printTable) where

-- |Print a table of data to stdout. You must supply the column width (number
--  of chars) and a list of row and column names.
printTable :: Show a =>
              Int       -- ^ Column width
           -> [[a]]     -- ^ Data
           -> [String]  -- ^ Column names (including the 0th column)
           -> [String]  -- ^ Row names
           -> IO ()
printTable pad xs header rownames = do
    let horzLines = replicate (length header) (replicate pad '-')
    printRow pad '+' horzLines
    printRow pad '|' header
    printRow pad '+' horzLines
    let rows = zipWith (:) rownames (map (map show) xs)
    mapM_ (printRow pad '|') rows
    printRow pad '+' horzLines

-- |Print a single row of a table.
printRow :: Int -> Char -> [String] -> IO ()
printRow pad sep xs = do
    let ys = map (trim pad) xs
    putChar sep
    mapM_ printCell ys
    putStrLn ""
    where
        trim pad str = let n = length str
                           m = max 0 (pad - n)
                        in take pad str ++ replicate m ' '
        printCell cl = putStr cl >> putChar sep