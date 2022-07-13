module Main
  ( module BoolConj
  , module BoolDisj
  , module Combine
  , main
  ) where

import BoolConj
import BoolDisj
import Combine
import Four
import Identity
import Or
import Two
import Test.QuickCheck
import Three
import Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

combineAssoc f g h x =
  unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (combineAssoc   :: CombineAssoc)
