module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : f 0 1
  where f a b = (a + b) : f b (a + b)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a rest) = a : streamToList rest

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a rest) = Cons (f a) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: (Stream a) -> (Stream a) -> (Stream a)
interleaveStreams (Cons x xs) (Cons y ys) =
  Cons x (Cons y (interleaveStreams xs ys))

ruler :: Stream Integer
ruler = undefined
  
rulern n =
  length $ takeWhile (\x -> n `mod` x == 0) $ iterate (*2) 2

ruler' n = (rulern n) : ruler' (n+1)


