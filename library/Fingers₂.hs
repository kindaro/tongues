module Fingers₂ where

import Prelude.Fancy

import Data.Functor.Foldable
import Data.Fix

data Saying recurse= Abstraction recurse | Application recurse recurse | Reference ℕ deriving (Show, Eq, Ord, Generic)

data Rule saying = Rule Text (saying (Fix saying) → Maybe (Fix saying))
data Log saying = Log {source, target ∷ saying, rule ∷ Text} deriving (Show, Eq, Ord, Generic)

inwrite ∷ ℕ → Saying → Saying → Saying
inwrite target filling reference@(Reference this)
  | this ≡ target = filling
  | otherwise = reference
inwrite target filling (Application function argument) = Application (inwrite target filling function) (inwrite target filling argument)
inwrite target filling (Abstraction abstracted) = Abstraction (inwrite (target + 1) (bump filling) abstracted)

rulesForEasing ∷ [Rule Saying]
rulesForEasing = writ id do
  say do Rule "live application" \ case
           Application (Fix (Abstraction abstracted)) argument → (Just ∘ Fix) do (inwrite 0 argument abstracted)
           _ → Nothing
  -- say do Rule "dead application" \ case
  --          Application function argument → Application (ease function) (ease argument)
  --          _ → Nothing
  -- say do Rule "dead application" \ case
  --          Application function argument → Application (ease function) (ease argument)
  --          _ → Nothing
-- ease (Application (ease → Abstraction abstracted) argument) = slump (ease (inwrite 0 argument abstracted))
-- ease (Application function argument) = Application (ease function) (ease argument)
-- ease (Abstraction abstracted) = Abstraction (ease abstracted)
-- ease reference@(Reference _) = reference
