module AI.Search.Local
    ( hillClimbingSearch
    , Schedule(..)
    , expSchedule
    , simulatedAnnealing
    ) where

import AI.Search.Core
import AI.Util.Util

-------------------
-- Hill Climbing --
-------------------

-- |From the initial node, keep choosing the neighbour with the highest value,
--  stopping when no neighbour is better.
hillClimbingSearch :: (Problem p s a) => p s a -> Node s a
hillClimbingSearch prob = go (root prob)
    where
        go node = if value neighbour <= value node
            then node
            else go neighbour
            where
                neighbour = argMax (expand prob node) value

-------------------------
-- Simulated Annealing --
-------------------------

-- |Data type for an annealing schedule.
type Schedule = Int -> Double

-- |One possible schedule function for simulated annealing.
expSchedule :: Double -> Int -> Schedule
expSchedule lambda limit k = if k < limit
    then exp (-lambda * fromIntegral k)
    else 0

-- |Simulated annealing search. At each stage a random neighbour node is picked,
--  and we move to that node if its value is higher than the current
--  node. If its value is lower, then we move to it with some probability
--  depending on the current 'temperature'. The temperature is gradually
--  reduced according to an annealing schedule, making random jumps less likely
--  as the algorithm progresses.
simulatedAnnealing :: (Problem p s a) => Schedule -> p s a -> IO (Node s a)
simulatedAnnealing schedule prob = go 0 (root prob)
    where
        go k current = let t = schedule k in
            if t == 0
                then return current
                else do
                    next <- randomChoiceIO (expand prob current)
                    let deltaE = value next - value current
                    jump <- probabilityIO (exp $ deltaE / t)
                    if deltaE > 0 || jump
                        then go (k+1) next
                        else go (k+1) current
