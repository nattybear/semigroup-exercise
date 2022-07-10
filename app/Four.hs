module Four where

import Test.QuickCheck

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

instance (Semigroup a,
          Semigroup b,
          Semigroup c,
          Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four e f g h = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c,
          Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type F = Four String String String String
type FourAssoc = F -> F -> F -> Bool
