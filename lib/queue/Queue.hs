module Queue (Queue, mkQueue, Queue.null, Queue.head, Queue.tail, snoc) where

data Queue a = Queue [a] [a]

mkQueue :: Queue a
mkQueue = Queue [] []

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

head :: Queue a -> Maybe a
head (Queue [] (h:_)) = Just h
head q
  | Queue.null q = Nothing
  | otherwise = Queue.head $ pour q

tail :: Queue a -> Queue a
tail (Queue [] (_:t)) = Queue [] t
tail q
  | Queue.null q = mkQueue
  | otherwise = Queue.tail $ pour q

snoc :: a -> Queue a -> Queue a
snoc a (Queue l r) = Queue (a : l) r

pour :: Queue a -> Queue a
pour (Queue l r) = Queue [] (reverse l ++ r)
