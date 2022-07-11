module BoolConj where

import Test.QuickCheck

newtype BoolConj = BoolConj Bool
                 deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _             <> _             = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

type B = BoolConj
type BoolConjAssoc = B -> B -> B -> Bool
