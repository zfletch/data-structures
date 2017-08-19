module Stack (Stack, mkStack, Stack.null, Stack.head, Stack.tail, cons) where

data Stack a = Head a (Stack a) | Empty

mkStack :: Stack a
mkStack = Empty

null :: Stack a -> Bool
null (Head _ _) = False
null Empty = True

head :: Stack a -> Maybe a
head (Head h _) = Just h
head Empty = Nothing

tail :: Stack a -> Stack a
tail (Head _ t) = t
tail Empty = Empty

cons :: a -> Stack a -> Stack a
cons a s = Head a s
