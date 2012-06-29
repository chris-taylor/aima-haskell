module Graph where

import Data.Map (Map)
import qualified Data.Map as M

type Graph a b = Map a (Map a b)

toGraph :: (Ord a) => [(a, [(a,b)])] -> Graph a b
toGraph xs = M.fromList (map f xs)
    where
        f (a,bs) = (a, M.fromList bs)

fromGraph :: Graph a b -> [(a, [(a,b)])]
fromGraph xs = map g (M.toList xs)
    where
        g (a,bs) = (a, M.toList bs)

toPairRep :: Graph a b -> [(a,a,b)]
toPairRep xs = [ (a,b,c) | (a,bs) <- fromGraph xs, (b,c) <- bs ]

symmetrize :: [(a,a,b)] -> [(a,a,b)]
symmetrize xs = concat [ [(a,b,c),(b,a,c)] | (a,b,c) <- xs ] 

fromPairRep :: (Ord a) => [(a,a,b)] -> Graph a b
fromPairRep xs = go xs M.empty
    where
        go []           m = m
        go ((a,b,c):xs) m = go xs (M.insert a newMap m)
            where
                newMap = M.insert b c $ case M.lookup a m of
                    Nothing -> M.empty
                    Just m' -> m'

toUndirectedGraph :: Ord a => [(a,[(a,b)])] -> Graph a b
toUndirectedGraph conn = fromPairRep . symmetrize . toPairRep $ toGraph conn

getNeighbours :: Ord a => a -> Graph a b -> [(a,b)]
getNeighbours a g = case M.lookup a g of
    Nothing -> error "Vertex not found in graph!"
    Just ls -> M.toList ls