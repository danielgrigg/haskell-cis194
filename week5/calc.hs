{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Calc where

import ExprT
import Parser (parseExp)
import qualified StackVM as VM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just e -> Just (eval e)
  Nothing -> Nothing
  
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  add a b = Add a b
  mul a b = Mul a b

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit n = n
  add a b = a + b
  mul a b = a * b
  
instance Expr Bool where
  lit n = if n <= 0 then False else True
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 n
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr VM.Program where
  lit n = [VM.PushI n]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
