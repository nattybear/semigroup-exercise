module Combine where

import Data.Monoid
import Test.QuickCheck
import Text.Show.Functions

newtype Combine a b = Combine { unCombine :: (a -> b) }
                    deriving Show

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary b) =>
          Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

type C            = Combine Int (Sum Int)
type CombineAssoc = C -> C -> C -> Int -> Bool
