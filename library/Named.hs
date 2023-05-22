module Named where

import Niceties
import Prelude.Fancy

import Data.String

data Saying = Λ Mark Saying | Saying :@ Saying | Variable Mark
infixl 9 :@
instance IsString Saying where fromString = Variable ∘ fromString
instance At Saying where (@) = (:@)
instance WingedWords Saying where
  y = Λ"f" (Λ"x" ("f" @ "x" @ "x") @ Λ"x" ("f" @ "x" @ "x"))
  k = Λ"x" (Λ"y" "x")
  zero = Λ"x" (Λ"y" "y")
  s = Λ"x" (Λ"y" (Λ"z" ("x" @ "z" @ ("y" @ "z"))))
  i = s @ k @ zero


