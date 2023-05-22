module Niceties where

import Prelude.Fancy
import Prelude.Fancy.QuickCheck

import Data.Char
import Data.Text qualified as Text
import Data.String

newtype Mark = Mark {mark ∷ Text} deriving newtype (Show, Eq, Ord)
instance IsString Mark where fromString = Mark ∘ Text.pack

instance Arbitrary Mark where
  arbitrary = do
    let letter = for arbitrary \ number → chr do 97 + number `mod` 26
    firstLetter ← letter
    otherLetters ← listOf letter
    pure do (Mark ∘ Text.pack) (firstLetter: otherLetters)
  shrink (Mark (Text.unpack → head: tail)) = fmap (Mark ∘ Text.pack) do zipWith (:) (shrink head) (shrink tail)
  shrink _ = error "Impossible: marks are never zero length."

(!?) :: [α] -> ℕ -> Maybe α
[ ] !? _ = Nothing
(α: _) !? 0 = Just α
(_: αs) !? farness = αs !? (farness − 1)
