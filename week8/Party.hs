{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where
import Data.Monoid
import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons (e@Emp { empFun = femp}) (GL emps fall) = GL (e:emps) (femp + fall)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es f) (GL es' f') = GL (es ++ es') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl0@(GL _ f0) gl1@(GL _ f1)
  | f0 >= f1 = gl0
  | otherwise = gl1

treeFold :: ([b] -> a -> b) -> Tree a -> b
treeFold f (Node { rootLabel = x, subForest = [] }) = f [] x
treeFold f (Node { rootLabel = x, subForest = children }) =
  f (map (treeFold f) children) x
  

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, 
                                                      GuestList)
nextLevel = undefined
