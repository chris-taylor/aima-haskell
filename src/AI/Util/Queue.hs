module Queue where

-- |An abstract Queue class supporting a test for emptiness and push/pop
--  functions. You can override the function 'extend' for performance reasons.
class Queue q where
    empty  :: q a -> Bool
    pop    :: q a -> (a, q a)
    push   :: a -> q a -> q a

    extend :: [a] -> q a -> q a
    extend xs q = foldr push q xs

-- |Lists can represent LIFO queues if 'push' conses new elements onto the
--  front of the queue.
instance Queue [] where
    empty  = null
    pop q  = (head q, tail q)
    push   = (:)
    extend = (++)

-- |A FIFO queue implemented with a list. Note that this is inefficient, as the
--  complexity is O(n) to push a new element onto the list.
instance Queue FifoQueue where
    empty     (FifoQueue q) = null q
    pop       (FifoQueue q) = (head q, FifoQueue (tail q))
    push x    (FifoQueue q) = FifoQueue (q ++ [x])
    extend xs (FifoQueue q) = FifoQueue (q ++ xs)

data FifoQueue a = FifoQueue [a]

-- |A priority queue implemented as an association list. This is inefficient
--  as the complexity of push is O(n), as opposed to O(log n) if we used a
--  tree representation.
instance Ord k => Queue (PriorityQueue k) where
    empty  (PQueue q _) = null q
    pop    (PQueue q f) = (snd (head q), PQueue (tail q) f)
    push x (PQueue q f) = PQueue (ins (f x) x q) f
        where
            ins k x []              = [(k, x)]
            ins k x ((k',v) : rest) = if k < k'
                then (k,x) : (k',v) : rest
                else (k',v) : ins k x rest

data PriorityQueue k a = PQueue { pqueue :: [(k,a)], keyfun :: a -> k }
