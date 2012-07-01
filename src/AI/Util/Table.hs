module AI.Util.Table (printTable) where

printTable :: Show a => Int -> [[a]] -> [String] -> [String] -> IO ()
printTable pad xs header rownames = do
    let horzLines = replicate (length header) (replicate pad '-')
    printRow pad horzLines
    printRow pad header
    printRow pad horzLines
    let rows = zipWith (:) rownames (map (map show) xs)
    mapM_ (printRow pad) rows
    printRow pad horzLines

printRow :: Int -> [String] -> IO ()
printRow pad xs = do
    let ys = map (trim pad) xs
    putChar '|'
    mapM_ printCell ys
    putStrLn ""
    where
        trim pad str = let n = length str
                           m = max 0 (pad - n)
                        in take pad str ++ replicate m ' '
        printCell cl = putStr cl >> putChar '|'