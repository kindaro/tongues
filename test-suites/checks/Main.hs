{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Prelude.Fancy hiding (exponent, i, predecessor)
import Prelude.Fancy.QuickCheck

import Fingers
import Niceties
import Renamingless qualified

import Generic.Random
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

instance Arbitrary Fingers.Saying where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

main = checks do
  checkPropertiesOf "boolean numbers" do
    checkProperty "true" \left right → ease (true @ left @ right) ↔ left
    checkProperty "false" \left right → ease (false @ left @ right) ↔ right
  checkPropertiesOf "natural numbers" do
    checkProperty "zero = false" do sayingOfNatural Zero ↔ false
    checkProperty "add = (+)" \n m → fixedPoint ease (add @ sayingOfNatural n @ sayingOfNatural m) ↔ fixedPoint ease (sayingOfNatural (n + m))
    checkProperty "compose = (×)" \n m → fixedPoint ease (sayingOfNatural n • sayingOfNatural m) ↔ fixedPoint ease (sayingOfNatural (n × m))
    checkProperty "exponentiate = (^)" \n m → let {base = n `mod` 4; exponent = m `mod` 6} in fixedPoint ease (exponentiate @ sayingOfNatural exponent @ sayingOfNatural base) ↔ fixedPoint ease (sayingOfNatural (base ^ exponent))
    checkProperty "predecessor" \n → let n' = n `mod` 10 in fixedPoint ease (predecessor @ sayingOfNatural n') ↔ sayingOfNatural if n' ≡ 0 then 0 else (n' − 1)
  checkPropertiesOf "Boolean numbers" do
    checkProperty "zero is zero" do fixedPoint ease (isZero @ sayingOfNatural 0) ↔ true
    checkProperty "successor is not zero" \n → fixedPoint ease (isZero @ sayingOfNatural (n + 1)) ↔ false
    checkProperty "if true" \left right → fixedPoint ease (conditional @ true @ left @ right) ↔ fixedPoint ease left
    checkProperty "if false" \left right → fixedPoint ease (conditional @ false @ left @ right) ↔ fixedPoint ease right
    checkProperty "twosome true" \left right → fixedPoint ease (twosome @ left @ right @ true) ↔ fixedPoint ease left
    checkProperty "twosome false" \left right → fixedPoint ease (twosome @ left @ right @ false) ↔ fixedPoint ease right
  checkPropertiesOf "category" do
    checkPropertiesOf "identity" do
      checkProperty "before" \f x → fixedPoint ease ((f • identity) @ x) ↔ fixedPoint ease (f @ x)
      checkProperty "after" \f x → fixedPoint ease ((identity • f) @ x) ↔ fixedPoint ease (f @ x)
    checkProperty "associativity" \f g h x → fixedPoint ease (((h • g) • f) @ x) ↔ fixedPoint ease ((h • (g • f)) @ x)
