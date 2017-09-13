{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee

-- add an employee to GuestList, updating cached fun score appropriately
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp name empfun) (GL es totalfun) = GL (e : es) (totalfun + empfun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend gl1@(GL es1 totalfun1) gl2@(GL es2 totalfun2) = GL (es1 ++ es2) (totalfun1 + totalfun2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | otherwise = gl2
