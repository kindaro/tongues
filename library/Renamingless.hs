module Renamingless where

-- import Prelude.Fancy
-- import Niceties
-- import Data.String
-- import Data.Text qualified as Text

-- data Saying = Λ Mark Saying | Saying :@ Saying | V Mark | N ℕ deriving (Show, Eq, Ord, Generic)

-- instance IsString Saying where fromString = V ∘ fromString
-- instance At Saying where (@) = (:@)
-- instance WingedWords Saying where
--   y = Λ"f" (Λ"x" ("f" @ "x" @ "x") @ Λ"x" ("f" @ "x" @ "x"))
--   k = Λ"x" (Λ"y" "x")
--   zero = Λ"x" (Λ"y" "y")
--   s = Λ"x" (Λ"y" (Λ"z" ("x" @ "z" @ ("y" @ "z"))))
--   i = s @ k @ zero

-- inwrite ∷ Mark → Saying → Saying → Saying
-- inwrite nameToInwrite inwriting = fix \ again → \case
--   Λ boundName body | boundName ≡ nameToInwrite → Λ boundName body | otherwise → Λ boundName (again body)
--   wending :@ spot → again wending :@ again spot
--   keep@(V nameHere) | nameHere ≡ nameToInwrite → inwriting | otherwise → keep
--   keep@(N _) → keep

-- fry ∷ Saying → Saying
-- fry = fix \again → \case
--   keep@(Λ _ _) → keep
--   V _ → error "Free variables are not supported!"
--   wending :@ spot → case again wending of
--     Λ hole saying → again do inwrite hole (again spot) saying
--     _ → error "Trying to apply something that is not a λ abstraction!"
--   keep@(N _) → keep
