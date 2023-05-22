module Fingers where

-- ^ Nicolas Govert de Bruijn's fingers au jus.

import Prelude.Fancy hiding (i)
import Niceties

import Data.Char
import Data.Text qualified as Text
import Data.String

data Saying = Λ Saying | Saying :@ Saying | V ℕ | Marker Mark deriving (Show, Eq, Ord, Generic)
infixl 9 :@
instance IsString Saying where fromString = Marker ∘ fromString
instance At Saying where {(@) = (:@)}
instance WingedWords Saying where
  y = Λ (Λ (V 1 @ V 0 @ V 0) @ Λ (V 1 @ V 0 @ V 0))
  k = Λ (Λ (V 1))
  zero = Λ (Λ (V 0))
  s = Λ (Λ (Λ (V 2 @ V 0 @ (V 1 @ V 0))))
  i = s @ k @ zero
instance Semigroup Saying where after <> before = Λ (Λ (Λ (V 2 @ (V 1 @ V 0)))) @ after @ before
instance Monoid Saying where mempty = i
class Λ_ saying where λ_ ∷ saying → saying
instance Λ_ Saying where λ_ = Λ
class V saying where v ∷ ℕ → saying
instance V Saying where v = V

newtype PrettySaying = PrettySaying {prettySaying ∷ Saying} deriving (Eq, Ord, Generic) deriving newtype (IsString)
instance Show PrettySaying where showsPrec precedence (PrettySaying saying) = showsPrecSaying precedence saying
deriving via Saying instance At PrettySaying
deriving via Saying instance WingedWords PrettySaying
deriving via Saying instance Λ_ PrettySaying
deriving via Saying instance V PrettySaying
deriving via Saying instance Semigroup PrettySaying
deriving via Saying instance Monoid PrettySaying

showsPrecSaying = (0 ∷ ℕ) & fix \ recurse depth precedence → \case
  Λ saying → showParen (precedence > 8) do (("λ" ++ [token (fromIntegral depth)] ++ ".") ++) ∘ recurse (depth + 1) 0 saying
  function :@ argument → showParen (precedence > 9) do recurse depth 9 function ∘ (" " ++) ∘ recurse depth 10 argument
  V finger → ([token (fromIntegral depth − fromIntegral finger − 1)] ++)
  Marker text → (((Text.unpack ∘ mark) text) ++)
  where token finger = chr (97 + ((finger + 23) `mod` 26))

bump ∷ Saying → Saying
  = 0 & fix \ recurse λdepth → \case
    Λ x → Λ (recurse (λdepth + 1) x)
    function :@ argument → recurse λdepth function :@ recurse λdepth argument
    V finger → if finger + 1 > λdepth then V (finger + 1) else V finger
    Marker text → Marker text

slump ∷ Saying → Saying
  = 0 & fix \ recurse λdepth → \case
    Λ x → Λ (recurse (λdepth + 1) x)
    function :@ argument → recurse λdepth function :@ recurse λdepth argument
    V finger → if finger > λdepth then V (finger − 1) else V finger
    Marker text → Marker text

step ∷ [Maybe Saying] → Saying → Saying
  = fix \ recurse context → \case
    Λ (e :@ V 0) → recurse context (slump e)  -- η
    Λ e :@ x → recurse (Just (recurse context x): context) e  -- β
    V finger → maybe (V (finger − fromIntegral (length (context)))) (maybe (V finger) id) do context !? finger
    Λ e → Λ do recurse (Nothing: (fmap ∘ fmap) bump context) e
    f :@ x → recurse context f @ recurse context x
    Marker text → Marker text

sauté ∷ Saying → [Saying] = converge ∘ iterate (step [ ])
fry ∷ PrettySaying → [PrettySaying] = fmap PrettySaying ∘ sauté ∘ prettySaying

simmer ∷ Saying → Saying
  = fix \ recurse → \case
  Λ (e :@ V 0) → slump e
  Λ e :@ x → inwrite 0 x e
  Λ e → Λ do recurse e
  f :@ x → recurse f :@ recurse x
  x → x

inwrite ∷ ℕ → Saying → Saying → Saying
  = fix \ recurse depth what → \case
    V finger | finger ≡ depth → what
    x@(V _) → x
    Λ e → Λ do recurse (depth + 1) what e
    f :@ x → recurse depth what f :@ recurse depth what x
    m@(Marker _) → m

glimmer ∷ PrettySaying → PrettySaying = PrettySaying ∘ simmer ∘ prettySaying
