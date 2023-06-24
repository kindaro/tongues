{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Prelude.Fancy hiding (exponent, predecessor)
import Prelude.Fancy.QuickCheck

import Fingers

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
    checkProperty "exponentiate = (^)" \n m → let base = n `mod` 4; exponent = m `mod` 6 in fixedPoint ease (exponentiate @ sayingOfNatural exponent @ sayingOfNatural base) ↔ fixedPoint ease (sayingOfNatural (base ^ exponent))
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
  checkPropertiesOf "functors" do
    checkPropertiesOf "twosome" do
      checkProperty "fmap id" \left right → fixedPoint ease (fmap_twosome @ identity @ (left # right)) ↔ fixedPoint ease (left # right)
      checkProperty "fmap compose" \left right f g → fixedPoint ease (fmap_twosome @ (g • f) @ (left # right)) ↔ fixedPoint ease ((fmap_twosome @ g • fmap_twosome @ f) @ (left # right))
    checkPropertiesOf "arrow" do
      checkProperty "fmap id" \f x → fixedPoint ease (fmap_arrow @ identity @ f @ x) ↔ fixedPoint ease (f @ x)
      checkProperty "fmap compose" \f g h x → fixedPoint ease (fmap_twosome @ (g • f) @ h @ x) ↔ fixedPoint ease ((fmap_twosome @ g • fmap_twosome @ f) @ h @ x)
  checkPropertiesOf "adjunctions" do
    checkProperty "φ identity = η" do fixedPoint ease (φ_twosome_arrow @ identity) ↔ fixedPoint ease η_state
    checkProperty "ψ identity = ε" do fixedPoint ease (ψ_twosome_arrow @ identity) ↔ fixedPoint ease ε_store
    checkProperty "μ from δ" do fixedPoint ease (fmap_arrow @ (ε_store • fmap_twosome @ ε_store • δ_twosome) • η_state) ↔ μ_arrow
    checkProperty "δ from μ" do fixedPoint ease (ε_store • fmap_twosome @ (μ_arrow • fmap_arrow @ η_state • η_state)) ↔ fixedPoint ease ((ψ_twosome_arrow • φ_twosome_arrow) @ δ_twosome)
