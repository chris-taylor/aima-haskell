module AI.Learning.DecisionTree where

import Data.Ord (comparing)
import qualified Data.List as L

-- |An attribute is anything that can split the data.
data Att a = Att { test :: a -> Bool
                 , label :: String }

instance Show (Att a) where
    show att = "Att(" ++ label att ++ ")"

-- |A decision tree which makes decisions based on attributes of type @a@ and
--  returns results of type @b@.
data DTree a b = Result b
               | Decision (Att a) b (DTree a b) (DTree a b)

instance Show b => Show (DTree a b) where
    show (Result b) = show b
    show (Decision att _ ts fs) =
        "Decision " ++ show att ++ " (" ++ show ts ++ ") (" ++ show fs ++ ")"

-- |Run the decision tree on an example
decide :: DTree a b -> a -> b
decide (Result b) _ = b
decide (Decision att _ tbranch fbranch) a =
    if test att a
    then decide tbranch a
    else decide fbranch a

instance Functor (DTree a) where
    fmap f (Result b) =
        Result (f b)
    fmap f (Decision att b tbranch fbranch) =
        Decision att (f b) (fmap f tbranch) (fmap f fbranch)

type Splitter a b = b -> Maybe (Att a,b,b)

-- |Repeatedly split nodes to form a decision tree. We leave the state
--  information at the branches so that the tree can be pruned later.
runSplitter :: Splitter a b -> b -> DTree a b
runSplitter split b = run b
    where
        run b = case split b of
            Nothing          -> Result b
            Just (att,b1,b2) -> Decision att b (run b1) (run b2)

-- |This is necessary because there is no 'Eq' instance for 'Attribute's, so
--  we can't delete them from a list.
--
--  >>> points [1,2,3]
--  [(1,[2,3]),(2,[1,3]),(3,[1,2])]
point :: [a] -> [(a,[a])]
point []     = []
point (a:as) = (a,as) : [ (b,a:bs) | (b,bs) <- point as ]

-- |Split a node of a decision tree by selecting the attribute that minimises
--  the function 'valf'.
minSplit :: Ord o => ([a] -> [a] -> o) -> Splitter a ([Att a],[a])
minSplit _    ([],_)    = Nothing
minSplit _    (_,[])    = Nothing
minSplit valf (atts,as) = if null choices
    then Nothing
    else Just (att, (atts',tlist), (atts',flist))
    where
        (att,atts',(tlist,flist)) =
            L.minimumBy (comparing (\(_,_,(ts,fs)) -> valf ts fs)) choices

        choices = filter (\(_,_,(ts,fs)) -> not (null ts || null fs)) $
            [(att,atts',L.partition (test att) as) | (att,atts') <- point atts]

-- |Prune a tree to have a maximum depth of decisions.
maxDecisions :: Int -> DTree a b -> DTree a b
maxDecisions i (Decision att b ts fs) =
    if i == 0
    then Result b
    else Decision att b (maxDecisions (i-1) ts) (maxDecisions (i-1) fs)
maxDecisions _ r = r

-- |Prune decisions using a predicate.
prune :: (b -> Bool) -> DTree a b -> DTree a b
prune _ r@(Result b) = r
prune p (Decision att b ts fs) = 
    if p b
    then Result b
    else Decision att b (prune p ts) (prune p fs)

-- |A useful function for the 'minSplit' routine.
entropy :: Ord a => [a] -> Float
entropy as = negate (entropy' probs)
    where
        entropy' ps = sum $ map (\p -> if p == 0 then 0 else p * log p) ps
        probs       = map ((/len) . fromIntegral . length) $ L.group $ L.sort as
        len         = fromIntegral (length as)

sumEntropy :: Ord b => (a -> b) -> [a] -> [a] -> Float
sumEntropy target a b = (entropy $ map target a) + (entropy $ map target b)
