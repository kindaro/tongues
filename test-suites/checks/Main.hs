{-# options_ghc -Wno-orphans #-}

module Main where

import Prelude.Fancy.QuickCheck
import Prelude.Fancy hiding (exponent, i)

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
    checkProperty "true" \ left right → ease (true @ left @ right) ↔ left
    checkProperty "false" \ left right → ease (false @ left @ right) ↔ right
  checkPropertiesOf "natural numbers" do
    checkProperty "zero = false" do sayingOfNatural Zero ↔ false
    checkProperty "add = (+)" \ n m → fixedPoint ease (add @ sayingOfNatural n @ sayingOfNatural m) ↔ fixedPoint ease (sayingOfNatural (n + m))
    checkProperty "compose = (×)" \ n m → fixedPoint ease (sayingOfNatural n • sayingOfNatural m) ↔ fixedPoint ease (sayingOfNatural (n × m))
    checkProperty "exponentiate = (^)" \ n m → let
      base = n `mod` 4
      exponent = m `mod` 6
      in fixedPoint ease (exponentiate  @ sayingOfNatural exponent @ sayingOfNatural base) ↔ fixedPoint ease (sayingOfNatural (base ^ exponent))
  checkPropertiesOf "category" do
    checkPropertiesOf "identity" do
      checkProperty "before" \ f x → fixedPoint ease ((f • identity) @ x) ↔ fixedPoint ease (f @ x)
      checkProperty "after" \ f x → fixedPoint ease ((identity • f) @ x) ↔ fixedPoint ease (f @ x)
    checkProperty "associativity" \ f g h x → fixedPoint ease (((h • g) • f) @ x) ↔ fixedPoint ease ((h • (g • f)) @ x)
