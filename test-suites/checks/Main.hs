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

deriving via Fingers.Saying instance Arbitrary Fingers.PrettySaying

instance Arbitrary Renamingless.Saying where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

main = defaultMain do
  writ (testGroup "") do
    (say ∘ writ (testGroup "renamingless capture-avoiding substitution")) do
      say do testProperty "fry works once" do Renamingless.fry ∘ Renamingless.fry ↔ Renamingless.fry
    (say ∘ writ (testGroup "fingers")) do
      (say ∘ testGroup "evaluators") do
        for ["fry" :× last ∘ Fingers.fry, "simmer" :× fixedPoint Fingers.glimmer] \ (name :× evaluator) →
          writ (testGroup name) do
            (say ∘ writ (testGroup "arithmetic")) do
              say do testProperty "zero" do evaluator (zero @ "f" @ "x") ↔ "x"
            (say ∘ writ (testGroup "fixed point")) do
              say do testProperty "y" do evaluator (y @ "f") ↔ evaluator ("f" @ (y @ "f"))
            (say ∘ writ (testGroup "SKI")) do
              say do testProperty "K" do evaluator (k @ "left" @ "right") ↔ "left"
              say do testProperty "SKSK = K" do \ same → evaluator (s @ k @ s @ same) ↔ evaluator same
              say do testProperty "SKx = I" do \ same anything → evaluator (s @ k @ anything @ same) ↔ evaluator (i @ same)
              say do testProperty "I = λx. x" do evaluator i ↔ evaluator (λ_ (v 0))
              say do testProperty "Ix = x" do \ x → evaluator (i @ x) ↔ evaluator x
            say do
              writ (testGroup "category") do
                say do testProperty "I ⊕ x = x" do \ x → evaluator (i ⊕ x) ↔ evaluator x
                say do testProperty "x ⊕ I = x" do \ x → evaluator (x ⊕ i) ↔ evaluator x
                say do testProperty "h ⊕ (g ⊕ f) = (h ⊕ g) ⊕ f" do \ h g f → evaluator (h ⊕ (g ⊕ f)) ↔ evaluator ((h ⊕ g) ⊕ f)


