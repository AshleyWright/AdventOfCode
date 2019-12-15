module PasswordCount2 where

import Prelude (class Show, Unit, map, show, ($), (<$>), (==))
import Data.Array (filter, foldl, group, length, range)
import Data.Array.NonEmpty (toArray)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Console (logShow)
import PasswordCount (ascendingDigits, matchesCriteria)
import Common ((∘), (∨))

repetitionExactLength ∷ ∀ a. Show a ⇒ Int → a → Boolean
repetitionExactLength count = (foldl (∨) false) ∘ (map (_ == count)) ∘ ((length ∘ toArray) <$> _) ∘ group ∘ toCharArray ∘ show

main ∷ Effect Unit
main = logShow $ length $ filter (matchesCriteria [ ascendingDigits, repetitionExactLength 2 ]) (range 240298 784956)
