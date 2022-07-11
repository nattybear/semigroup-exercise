module BoolDisj where

import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool
                 deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> _              = BoolDisj False
  BoolDisj True  <> BoolDisj False = BoolDisj False
  _              <> _              = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

type BD = BoolDisj
type BoolDisjAssoc = BD -> BD -> BD -> Bool
