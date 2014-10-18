module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) left right = Append (tag left <> tag right) left right

tag :: Monoid m => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _) = m
tag (Empty) = mempty

t0 = Single (Product 2) 'e'
t1 = Single (Product 3) 'a'

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a

indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append b l r) = case (size b, size . tag $ l) of
  (Size sb, Size sl) | n >= sb -> Nothing
                     | n < sl -> indexJ n l
                     | otherwise -> indexJ (n - sl) r 

t2 = Single (Size 1) 'e'
t3 = Single (Size 1) 'a'
t4 = Single (Size 1) 'y'
t5 = t2 +++ t3
t6 = t4 +++ (t2 +++ t3)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n s@(Single _ _)
  | n <= 0 = s
  | otherwise = Empty
               
dropJ n jl@(Append b l r)
  | n <= 0 = jl 
  | otherwise = case (size b, size . tag $ l) of
  (Size sb, Size sl) | n >= sb -> Empty
                     | n < sl -> dropJ n l
                     | otherwise -> dropJ (n - sl) r
  
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n s@(Single _ _)
  | n <= 0 = Empty
  | otherwise = s

takeJ n jl@(Append b l r)
  | n <= 0 = jl
  | otherwise = case (size b, size . tag $ l) of
    (Size sb, Size sl) | n >= sb -> jl
                       | n <= sl -> takeJ n l
                       | otherwise -> l +++ takeJ (n - sl) r
