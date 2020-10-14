module Data.Queue where

data Queue a = Queue
  { inbox :: [a],
    outbox :: [a],
    size :: Int
  }
  deriving (Show)

instance Eq a => Eq (Queue a) where
  (Queue i o s) == (Queue i' o' s')
    | s /= s' = False
    | i == i' && o == o' = True
    | otherwise = go i o == go i' o'
    where
      go i o = o ++ reverse i

enqueue :: a -> Queue a -> Queue a
enqueue a q@Queue {inbox = i, size = s} = q {inbox = a : i, size = s + 1}

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue q@Queue {inbox = [], outbox = []} = (q, Nothing)
dequeue q@Queue {inbox = i, outbox = []} = dequeue q {inbox = [], outbox = reverse i}
dequeue q@Queue {outbox = x : xs, size = s} = (q {outbox = xs, size = s - 1}, Just x)

empty :: Queue a
empty = Queue [] [] 0

null :: Queue a -> Bool
null (Queue [] [] 0) = True
null _ = False

singleton :: a -> Queue a
singleton = flip enqueue empty
