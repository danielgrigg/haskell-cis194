{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score x
  | x `elem` "aeilnorstu"  = Score 1
  | x `elem` "dg" = Score 2
  | x `elem` "bcmp" = Score 3
  | x `elem` "fhvwy" = Score 4
  | x `elem` "k" = Score 5
  | x `elem` "xj" = Score 8
  | x `elem` "qz" = Score 10
  | otherwise = Score 0
                    
scoreString :: String -> Score
scoreString = mconcat . map score
                                   

