module Queue (Queue, mkQueue, Queue.null, Queue.head, Queue.tail, snoc) where

data Queue a = Queue [a] [a]

mkQueue :: Queue a
mkQueue = Queue [] []

null :: Queue a -> Bool
null (Queue [] _) = True
null _ = False

head :: Queue a -> Maybe a
head (Queue (h:_) _) = Just h
head _ = Nothing

tail :: Queue a -> Queue a
tail (Queue (_:t) r) = checkf $ Queue t r
tail _ = mkQueue

snoc :: a -> Queue a -> Queue a
snoc a (Queue l r) = checkf $ Queue l (a:r)

checkf :: Queue a -> Queue a
checkf (Queue [] r) = Queue (reverse r) []
checkf q = q
