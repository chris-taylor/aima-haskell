module AI.Util.Util where

import qualified Data.List as L
import qualified Data.Ord as O

positiveInfinity :: Fractional a => a
positiveInfinity = 1/0

negativeInfinity :: Fractional a => a
negativeInfinity = -1/0

argMax :: (Ord b) => [a] -> (a -> b) -> a
argMax xs f = fst $ L.maximumBy (O.comparing snd) $ zip xs (map f xs)

argMin :: (Ord b) => [a] -> (a -> b) -> a
argMin xs f = fst $ L.minimumBy (O.comparing snd) $ zip xs (map f xs)

listToFunction :: (Eq a) => [(a,b)] -> a -> b
listToFunction xs x = case lookup x xs of
    Nothing -> error "Argument not found in list -- LISTTOFUNCTION"
    Just y  -> y
