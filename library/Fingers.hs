module Fingers where

-- ^ Nicolas Govert de Bruijn's fingers au jus.

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

inwrite ∷ ℕ → Saying → Saying → Saying
inwrite target filling reference@(Reference this)
  | this ≡ target = filling
  | otherwise = reference
inwrite target filling (Application function argument) = Application (inwrite target filling function) (inwrite target filling argument)
inwrite target filling (Abstraction abstracted) = Abstraction (inwrite (target + 1) (bump filling) abstracted)

overReferences ∷ (ℕ → ℕ → ℕ) → ℕ → Saying → Saying
overReferences mapping depth (Reference this) = Reference (mapping depth this)
overReferences mapping depth (Abstraction abstracted) = Abstraction (overReferences mapping (depth + 1) abstracted)
overReferences mapping depth (Application function argument) = Application (overReferences mapping depth function) (overReferences mapping depth argument)

bump ∷ Saying → Saying
bump = overReferences face 0

slump ∷ Saying → Saying
slump = overReferences degeneracy 0

face ∷ ℕ → ℕ → ℕ
face threshold number | number ≥ threshold = number + 1 | otherwise = number

(@) ∷ Saying → Saying → Saying
function @ argument = Application function argument

degeneracy ∷ ℕ → ℕ → ℕ
degeneracy threshold number | number > threshold = number − 1 | otherwise = number

ease ∷ Saying → Saying
ease (Application (ease → Abstraction abstracted) argument) = slump (inwrite 0 (bump argument) abstracted)
ease (Application function argument) = Application (ease function) (ease argument)
ease (Abstraction abstracted) = Abstraction (ease abstracted)
ease reference@(Reference _) = reference

sayingOfBoolean ∷ Bool → Saying
sayingOfBoolean False = false
sayingOfBoolean True = true

sayingOfNatural ∷ ℕ → Saying = fmap (Abstraction ∘ Abstraction) do
  fix \ recurse → \ case
    Zero → Reference Zero
    Successor n → Application (Reference 1) (recurse n)
