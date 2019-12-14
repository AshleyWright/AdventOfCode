module PasswordCount where

import Prelude (class Eq, class Ord, Unit, map, otherwise, show, ($), (==))
import Data.Array (filter, foldl, init, length, range, tail, zip)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Common ((∨), (∧), (∘), (∥), (≥), (≤))

matchesCriteria ∷ Array (Int → Boolean) → Int → Boolean
matchesCriteria criteria candidate = foldl (∧) true $ map (_ $ candidate) criteria

ascendingDigits ∷ Int → Boolean
ascendingDigits = (foldl (∧) true) ∘ (map doesAscend) ∘ (_ ∥ []) ∘ consecutives ∘ toCharArray ∘ show
  where
  doesAscend ∷ ∀ a. Ord a ⇒ Tuple a a → Boolean
  doesAscend (Tuple a b) = a ≤ b

equalAdjacents ∷ Int → Boolean
equalAdjacents = (foldl (∨) false) ∘ (map equal) ∘ (_ ∥ []) ∘ consecutives ∘ toCharArray ∘ show
  where
  equal ∷ ∀ a. Eq a ⇒ Tuple a a → Boolean
  equal (Tuple a b) = a == b

consecutives ∷ ∀ a. Array a → Maybe (Array (Tuple a a))
consecutives arr
  | length arr ≥ 2 = Just $ zip (init arr ∥ []) (tail arr ∥ [])
  | otherwise = Nothing

main ∷ Effect Unit
main = logShow $ length $ filter (matchesCriteria [ ascendingDigits, equalAdjacents ]) (range 240298 784956)
