{-# OPTIONS_GHC -Wno-orphans #-}

module Endly where

import Prelude.Fancy hiding (show)
import Prelude (Show(show))

import Data.Kind
import Data.Proxy

instance Show seemly ⇒ Show (Bool → seemly) where show ifly = "λx. x?" ++ show (ifly True) ++ ":" ++ show (ifly False)
instance Show seemly ⇒ Show (Integer → seemly) where show wending = show (fmap (\input → (input, wending input)) [0.. 5])

type Endly ∷ (★ → ★) → Constraint
class Endly representation where
  integerConstant ∷ Integer → representation Integer
  -- booleanConstant ∷ Bool → representation Bool
  λ ∷ (representation input → representation output) → representation (input → output)
  (@) ∷ representation (input → output) → representation input → representation output
  -- nail ∷ (representation variable → representation variable) → representation variable
  plus, (·) ∷ representation Integer → representation Integer → representation Integer
  minus ∷ representation Integer → representation Integer
  -- (≤) ∷ representation Integer → representation Integer → representation Bool
  -- (?) ∷ representation Bool → (representation variable, representation variable) → representation variable
  -- true, false ∷ representation Bool
  -- true = booleanConstant True
  -- false = booleanConstant False

-- example ∷ (Num (representation Integer), Endly representation) ⇒ representation (Integer → Integer → Integer)
-- example = λ \ x → nail \ self → λ \ y → (y ≤ 0) ? (1, x · (self @ (y + (-1))))

instance {-# overlapping #-} Endly representation ⇒ Num (representation Integer) where
  fromInteger = integerConstant
  negate = minus
  (+) = plus

instance Endly (Δ Integer) where
  integerConstant _ = Δ 0
  -- booleanConstant _ = Δ 0
  λ f = let Δ n = f (Δ 0) in Δ do n + 1
  Δ f @ Δ x = Δ do f `max` x + 1
  -- nail f = let Δ n = f (Δ 0) in Δ do n + 1
  plus (Δ this) (Δ that) = Δ do this `max` that + 1
  -- minus (Δ this) = Δ do this + 1
  Δ this · Δ that = Δ do this `max` that + 1
  -- Δ this ≤ Δ that = Δ do this `max` that + 1
  -- Δ this ? (Δ that, Δ other) = Δ do this `max` that `max` other + 1

data First variable where
  Variable ∷ Integer → First (Proxy anything)
  IntegerConstant ∷ Integer → First Integer
  -- BooleanConstant ∷ Bool → First Bool
  Λ ∷ First (Proxy input) → First output → First (input → output)
  (:@) ∷ First (input → output) → First input → First output
  -- Nail ∷ (First variable → First variable) → First variable
  (:+), (:·) ∷ First Integer → First Integer → First Integer
  Minus ∷ First Integer → First Integer
  -- (:≤) ∷ First Integer → First Integer → First Bool
  -- (:?) ∷ First Bool → (First variable, First variable) → First variable

-- instance Endly First where
--   integerConstant = IntegerConstant
--   -- true = BooleanConstant True
--   -- false = BooleanConstant False
--   λ = Λ
--   (@) = (:@)
--   -- nail = Nail
--   -- (+) = (:+)
--   (·) = (:·)
--   -- (≤) = (:≤)
--   -- (?) = (:?)
