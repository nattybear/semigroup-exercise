module Validation where

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Gen (oneof)

data Validation a b = Failure a
                    | Success b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success x <> Failure y = Success x
  Failure x <> Failure y = Failure $ x <> y
  Success x <> Success _ = Success x
  Failure _ <> Success y = Success y

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [ return $ Failure x
          , return $ Success y ]

type VSI = Validation String Int
type ValidationAssoc = VSI -> VSI -> VSI -> Bool

foo :: IO ()
foo = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
