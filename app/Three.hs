module Three where

import Test.QuickCheck

data Three a b c = Three a b c
                 deriving (Eq, Show)

data Three' a b = Three' a b b
                deriving (Eq, Show)

instance (Semigroup a,
          Semigroup b,
          Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Arbitrary a,
          Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x  <- arbitrary
    y  <- arbitrary
    y' <- arbitrary
    return (Three' x y y')

type T = Three String String String
type ThreeAssoc = T -> T -> T -> Bool
