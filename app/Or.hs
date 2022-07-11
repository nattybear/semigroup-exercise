module Or where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst x <> Snd y = Snd y
  Fst x <> Fst y = Fst y
  Snd x <> Fst y = Snd x
  Snd x <> Snd y = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [ return $ Fst x
          , return $ Snd y ]

type OSS = Or String String
type OrAssoc = OSS -> OSS -> OSS -> Bool
