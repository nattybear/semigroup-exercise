module BoolDisj where

import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool
                 deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj _     <> BoolDisj True  = BoolDisj True
  BoolDisj True  <> _              = BoolDisj True
  _              <> _              = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

type BD = BoolDisj
type BoolDisjAssoc = BD -> BD -> BD -> Bool
