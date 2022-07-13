module Comp where

import Data.Monoid
import Test.QuickCheck
import Text.Show.Functions

newtype Comp a = Comp { unComp :: (a -> a) }
               deriving Show

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ \x -> f x <> g x

instance Monoid a => Monoid (Comp a) where
  mempty = Comp $ \x -> mempty

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

type CS        = Comp (Sum Int)
type CompAssoc = CS -> CS -> CS -> Sum Int -> Bool
type CompMli   = CS -> Sum Int -> Bool
type CompMri   = CS -> Sum Int -> Bool

compAssoc f g h x =
  unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

compMli f x = unComp (mempty <> f) x == unComp f x

compMri f x = unComp (f <> mempty) x == unComp f x
