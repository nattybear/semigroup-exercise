module Main
  ( module BoolConj
  , module BoolDisj
  , module Combine
  , module Comp
  , module Mem
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
import Mem
import Or
import Pair
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

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f)
               => (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

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
  quickCheck (functorIdentity          :: Identity Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Identity Int -> Bool)
  quickCheck (functorIdentity          :: Pair Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Pair Int -> Bool)
  quickCheck (functorIdentity          :: Two Int Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Two Int Int -> Bool)
  quickCheck (functorIdentity          :: Three Int Int Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Three Int Int Int -> Bool)
  quickCheck (functorIdentity          :: Three' Int Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Three' Int Int -> Bool)
  quickCheck (functorIdentity          :: Four Int Int Int Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Four Int Int Int Int -> Bool)
  quickCheck (functorIdentity          :: Four' Int Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Four' Int Int -> Bool)
