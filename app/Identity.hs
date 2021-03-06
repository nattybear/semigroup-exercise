module Identity where

import Test.QuickCheck

newtype Identity a = Identity a
                 deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool
