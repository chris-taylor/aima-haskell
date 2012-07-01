module AI.Util.Util where

import qualified Data.List as L
import qualified Data.Ord as O

argMax :: (Ord b) => [a] -> (a -> b) -> a
argMax xs f = fst $ L.maximumBy (O.comparing snd) $ zip xs (map f xs)

argMin :: (Ord b) => [a] -> (a -> b) -> a
argMin xs f = fst $ L.minimumBy (O.comparing snd) $ zip xs (map f xs)