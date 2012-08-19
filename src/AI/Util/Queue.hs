module AI.Util.Queue
    (
    -- * Type class and queue functions
    Queue (..)
    , notEmpty
    -- * Queue instances
    , FifoQueue
    , PriorityQueue
    , newPriorityQueue
    ) where

import qualified Data.Map as M

-- |An abstract Queue class supporting a test for emptiness and push/pop
--  functions. You can override the function 'extend' for performance reasons.
class Queue q where
    -- |Return an empty queue.
    newQueue :: q a

    -- |Return 'True' if the queue is empty.
    empty  :: q a -> Bool

    -- |Pop an element from the front of the queue, also returning
    --  the remaining queue.
    pop    :: q a -> (a, q a)

    -- |Push a new element into the queue.
    push   :: a -> q a -> q a

    -- |Push a list of elements into the queue one by one.
    extend :: [a] -> q a -> q a
    extend xs q = foldr push q xs

-- |Return 'True' if a queue has any elements remaining.
notEmpty :: Queue q => q a -> Bool
notEmpty = not . empty

-- |Lists can represent LIFO queues if 'push' conses new elements onto the
--  front of the queue.
instance Queue [] where
    newQueue = []
    empty  = null
    pop q  = (head q, tail q)
    push   = (:)
    extend = (++)

-- |An amortized O(1) FIFO queue. We maintain a fast push operation by storing
--  the front and back of the queue in separate lists. Whenever the front of the
--  queue is empty, we reverse the back of the queue and put the reversed list
--  at the front. Although it takes O(n) time to reverse the list, each element
--  only needs to be moved once, and so the amortized time is O(1).
--
--  Code adapted from Eric Kidd:
--  <http://www.randomhacks.net/articles/2007/02/08/haskell-queues-without-pointers>
instance Queue FifoQueue where
    newQueue = FifoQueue [] []

    empty     (FifoQueue [] []) = True
    empty     _                 = False

    pop       (FifoQueue [] [])     = error "Can't pop from an empty queue"
    pop       (FifoQueue (x:xs) ys) = (x, FifoQueue xs ys)
    pop       (FifoQueue [] ys)     = pop (FifoQueue (reverse ys) [])

    push y    (FifoQueue xs ys) = FifoQueue xs (y:ys)

data FifoQueue a = FifoQueue [a] [a]

-- |A priority queue implemented as a map. Both pop and push have O(log n)
--  complexity.
instance Ord k => Queue (PriorityQueue k) where
    newQueue = undefined
    empty  (PQueue q _) = M.null q
    pop    (PQueue q f) = (snd minAssoc,PQueue rest f)
        where (minAssoc,rest) = M.deleteFindMin q
    push x (PQueue q f) = PQueue (M.insert (f x) x q) f

data PriorityQueue k a = PQueue { pqueue :: M.Map k a, keyfun :: a -> k }

newPriorityQueue :: (a -> k) -> PriorityQueue k a
newPriorityQueue f = PQueue M.empty f
