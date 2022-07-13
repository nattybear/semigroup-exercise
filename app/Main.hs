module Main
  ( module BoolConj
  , module BoolDisj
  , module Combine
  , module Comp
  , module Validation
  , main
  ) where

import BoolConj
import BoolDisj
import Combine
import Comp
import Data.Monoid
import Four
import Identity
import Or
import Two
import Test.QuickCheck
import Three
import Trivial
import Validation

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (semigroupAssoc      :: TrivAssoc)
  quickCheck (semigroupAssoc      :: IdAssoc)
  quickCheck (semigroupAssoc      :: TwoAssoc)
  quickCheck (semigroupAssoc      :: ThreeAssoc)
  quickCheck (semigroupAssoc      :: FourAssoc)
  quickCheck (semigroupAssoc      :: BoolConjAssoc)
  quickCheck (semigroupAssoc      :: BoolDisjAssoc)
  quickCheck (semigroupAssoc      :: OrAssoc)
  quickCheck (combineAssoc        :: CombineAssoc)
  quickCheck (compAssoc           :: CompAssoc)
  quickCheck (semigroupAssoc      :: ValidationAssoc)
  quickCheck (monoidLeftIdentity  :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity  :: Identity Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Identity Trivial -> Bool)
  quickCheck (monoidLeftIdentity  :: Two Trivial Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Two Trivial Trivial -> Bool)
  quickCheck (monoidLeftIdentity  :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity  :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (combineMli          :: CombineMli)
  quickCheck (combineMri          :: CombineMri)
  quickCheck (compMri             :: CompMri)
  quickCheck (compMri             :: CompMri)
