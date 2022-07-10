module Identity where

import Test.QuickCheck

newtype Identity a = Identity a
                 deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool
