module Combine where

import Data.Monoid
import Test.QuickCheck
import Text.Show.Functions

newtype Combine a b = Combine { unCombine :: (a -> b) }
                    deriving Show

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

instance (CoArbitrary a, Arbitrary b) =>
          Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

type C            = Combine Int (Sum Int)
type CombineAssoc = C -> C -> C -> Int -> Bool
type CombineMli   = C -> Int -> Bool
type CombineMri   = C -> Int -> Bool

combineAssoc f g h x =
  unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

combineMli f x = unCombine (mempty <> f) x == unCombine f x

combineMri f x = unCombine (f <> mempty) x == unCombine f x
