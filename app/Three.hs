module Three where

import Test.QuickCheck

data Three a b c = Three a b c
                 deriving (Eq, Show)

instance (Semigroup a,
          Semigroup b,
          Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

type T = Three String String String
type ThreeAssoc = T -> T -> T -> Bool
