{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuantifiedConstraints, TypeApplications, ScopedTypeVariables #-}
import Test.QuickCheck
import Test.QuickCheck.Classes.Base
import Data.Proxy
import Control.Monad
import Test.Tasty
import Test.Tasty.QuickCheck

import Lib

instance Arbitrary a => Arbitrary (Singleton a) where
  arbitrary = Singleton <$> arbitrary
  shrink = traverse shrink

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [
      pure NoValue
    , HasValue <$> arbitrary
    ]
  shrink = traverse shrink

instance Arbitrary a => Arbitrary (NotQuiteList a) where
  arbitrary = sized $ \n -> do
    v <- Value <$> arbitrary
    pure . foldr ($) v $ replicate n Layer
  shrink = traverse shrink

instance Arbitrary a => Arbitrary (NotEmpty a) where
  arbitrary = sized $ \n -> do
    ~(h:t) <- replicateM (n+1) arbitrary
    pure $ foldr MidValue (LastValue h) t
  shrink = traverse shrink

instance (Arbitrary a, Arbitrary b) => Arbitrary (Productish a b) where
  arbitrary = Productish <$> arbitrary <*> arbitrary
  shrink = traverse shrink

instance (Arbitrary a, Arbitrary b) => Arbitrary (Summish a b) where
  arbitrary = oneof [
      First <$> arbitrary
    , Second <$> arbitrary
    ]
  shrink = traverse shrink

main :: IO ()
main = defaultMain $ testGroup "Laws" [
    check @Singleton           Proxy "Singleton"
  , check @(Productish String) Proxy "Productish"
  , check @(Summish Int)       Proxy "Summish"
  , check @Optional            Proxy "Optional"
  , check @NotQuiteList        Proxy "NotQuiteList"
  , check @NotEmpty            Proxy "NotEmpty"
  ]

check :: forall f.
  ( Functor f
  , Applicative f
  , Monad f
  , Foldable f
  , Traversable f
  , forall a. Eq a => Eq (f a)
  , forall a. Show a => Show (f a)
  , forall a. Arbitrary a => Arbitrary (f a)
  ) => Proxy f -> String -> TestTree
check _ name = testGroup name . map (go . ($ Proxy)) $ lawsToCheck
  where
  go (Laws className properties) = testProperties className properties
  lawsToCheck :: [Proxy f -> Laws]
  lawsToCheck = [
      functorLaws
    , applicativeLaws
    , monadLaws
    , traversableLaws
    , foldableLaws
    ]
