-- | Nicolas Govert de Bruijn's fingers au jus.
module Fingers where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Prelude.Fancy

data Saying = Abstraction Saying | Application Saying Saying | Reference ℕ deriving (Show, Eq, Ord, Generic)

recursion = Abstraction do
  Application
    do Abstraction do Reference 1 `Application` Reference 0 `Application` Reference 0
    do Abstraction do Reference 1 `Application` Reference 0 `Application` Reference 0
true = (Abstraction ∘ Abstraction ∘ Reference) 1
false = (Abstraction ∘ Abstraction ∘ Reference) 0
apply = (Abstraction ∘ Abstraction ∘ Abstraction) do
  Reference 2 `Application` Reference 0 `Application` (Reference 1 `Application` Reference 0)
identity = apply `Application` true `Application` false

overReferences ∷ (ℕ → ℕ → ℕ) → ℕ → Saying → Saying
overReferences mapping depth (Reference this) = Reference (mapping depth this)
overReferences mapping depth (Abstraction abstracted) = Abstraction (overReferences mapping (depth + 1) abstracted)
overReferences mapping depth (Application function argument) = Application (overReferences mapping depth function) (overReferences mapping depth argument)

face ∷ ℕ → ℕ → ℕ
face threshold number | number ≥ threshold = number + 1 | otherwise = number

degeneracy ∷ ℕ → ℕ → ℕ
degeneracy threshold number | number > threshold = number − 1 | otherwise = number

bump ∷ Saying → Saying
bump = overReferences face 0

slump ∷ Saying → Saying
slump = overReferences degeneracy 0

inwrite ∷ ℕ → Saying → Saying → Saying
inwrite target filling reference@(Reference this)
  | this ≡ target = filling
  | otherwise = reference
inwrite target filling (Application function argument) = Application (inwrite target filling function) (inwrite target filling argument)
inwrite target filling (Abstraction abstracted) = Abstraction (inwrite (target + 1) (bump filling) abstracted)

ease ∷ Saying → Saying
ease (Application (ease → Abstraction abstracted) argument) = slump (inwrite 0 (bump argument) abstracted)
ease (Application function argument) = Application (ease function) (ease argument)
ease (Abstraction abstracted) = Abstraction (ease abstracted)
ease reference@(Reference _) = reference

sayingOfBoolean ∷ Bool → Saying
sayingOfBoolean False = false
sayingOfBoolean True = true

sayingOfNatural ∷ ℕ → Saying = fmap (Abstraction ∘ Abstraction) do
  fix \recurse → \case
    Zero → Reference Zero
    Successor n → Application (Reference 1) (recurse n)

add ∷ Saying
add = (Abstraction ∘ Abstraction ∘ Abstraction) (compose `Application` Application (Reference 2) (Reference 0) `Application` Application (Reference 1) (Reference 0))

compose ∷ Saying
compose = (Abstraction ∘ Abstraction ∘ Abstraction) (Application (Reference 2) (Application (Reference 1) (Reference 0)))

exponentiate ∷ Saying
exponentiate = (Abstraction ∘ Abstraction) (Reference 1 `Application` (compose `Application` (Reference 0)) `Application` sayingOfNatural 1)

isZero ∷ Saying
isZero = Abstraction (Reference 0 `Application` (true `Application` false) `Application` true)

conditional ∷ Saying
conditional = (Abstraction ∘ Abstraction ∘ Abstraction) (Reference 2 `Application` Reference 1 `Application` Reference 0)

twosome ∷ Saying
twosome = (Abstraction ∘ Abstraction ∘ Abstraction) (conditional `Application` Reference 0 `Application` Reference 2 `Application` Reference 1)

predecessor ∷ Saying
-- λn. λfx.
-- n (λx'.
-- if (isZero (π₀ x'))
-- then (1 :× π₁ x')
-- else (π₀ x' :× f (π₁ x')))
-- (0 :× x)
predecessor =
  (Abstraction ∘ Abstraction ∘ Abstraction)
    ( ( Reference 2
          `Application` ( Abstraction
                            ( conditional
                                `Application` (isZero `Application` (Reference 0 `Application` true))
                                `Application` (twosome `Application` sayingOfNatural 1 `Application` (Reference 0 `Application` false))
                                `Application` (twosome `Application` (Reference 0 `Application` true) `Application` (Reference 2 `Application` (Reference 0 `Application` false)))
                            )
                        )
          `Application` (twosome `Application` sayingOfNatural 0 `Application` Reference 0)
      )
        `Application` false
    )

(@) ∷ Saying → Saying → Saying
function @ argument = Application function argument
infixl 9 @

(•) ∷ Saying → Saying → Saying
after • before = compose @ after @ before
infixl 8 •

λ ∷ Saying → Saying
λ = Abstraction

x ∷ ℕ → Saying
x = Reference

-- | Tuple constructor.
(#) ∷ Saying → Saying → Saying
left # right = twosome @ left @ right

λπ₀ ∷ Saying → Saying
λπ₀ tuple = tuple @ true
λπ₁ ∷ Saying → Saying
λπ₁ tuple = tuple @ false

ε_twosome, δ_twosome, ε_store, η_state, fmap_twosome, fmap_arrow ∷ Saying
ε_twosome = λ (x 0 @ true)
δ_twosome = λ (x 0 # λπ₁ (x 0))
ε_store = λ (λπ₀ (x 0) @ λπ₁ (x 0))
η_state = λ (λ (x 1 # x 0))
φ_twosome_arrow = λ (fmap_arrow @ x 0 • η_state)
ψ_twosome_arrow = λ (ε_store • fmap_twosome @ x 0)
η_arrow = true
μ_arrow = λ (λ (x 1 @ x 0 @ x 0))
fmap_twosome = (λ ∘ λ) (x 1 @ (x 0 @ true) # (x 0 @ false))
fmap_arrow = (λ ∘ λ) (x 1 • x 0)

prettyPrint ∷ Saying → IO ()
prettyPrint =
  Text.putStrLn ∘ fix \recurse → \case
    Abstraction (Reference 0 `Application` left `Application` right) → parenthesize (Text.unwords [recurse (slump left), "#", recurse (slump right)])
    Abstraction saying → Text.unwords ["λ.", recurse saying]
    Application function argument → writ Text.unwords do
      say do if hasRightmostLambda function then parenthesize (recurse function) else recurse function
      say do case argument of { (Application _ _) → parenthesize (recurse argument); _ → recurse argument }
    Reference n → show n
 where
  hasRightmostLambda ∷ Saying → 𝔹
  hasRightmostLambda (Abstraction _) = True
  hasRightmostLambda (Application _ argument) = hasRightmostLambda argument
  hasRightmostLambda (Reference _) = False

  parenthesize ∷ Text → Text
  parenthesize text = "(" ⊕ text ⊕ ")"
