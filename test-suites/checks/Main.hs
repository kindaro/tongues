{-# options_ghc -Wno-orphans #-}

module Main where

import Prelude.Fancy.QuickCheck
import Prelude.Fancy hiding (i)

import Niceties
import Fingers
import Renamingless qualified

import Test.QuickCheck.Instances.Natural ( )
import Test.QuickCheck.Instances.Text ( )
import Generic.Random

instance Arbitrary Fingers.Saying where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

main = checks do
  checkPropertiesOf "boolean numbers" do
    checkProperty "true" \ left right → ease (true `Application` left `Application` right) ↔ left
    checkProperty "false" \ left right → ease (false `Application` left `Application` right) ↔ right
  checkPropertiesOf "natural numbers" do
    checkProperty "zero = false" do sayingOfNatural Zero ↔ false
