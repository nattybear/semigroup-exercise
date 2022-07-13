module Comp where

import Data.Monoid
import Test.QuickCheck
import Text.Show.Functions

newtype Comp a = Comp { unComp :: (a -> a) }
               deriving Show

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

type CS = Comp (Sum Int)
type CompAssoc = CS -> CS -> CS -> Sum Int -> Bool
