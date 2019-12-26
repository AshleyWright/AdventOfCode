module Common where

import Prelude (append, bind, compose, conj, disj, div, flip, mod, mul, notEq, pure, ($))
import Data.Array (fromFoldable, index, difference)
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe, fromMaybe)
import Data.Ord (greaterThanOrEq, lessThanOrEq)
import Data.Tuple (Tuple(..))
import Data.Vec (cons)
import Effect
import Foreign (MultipleErrors)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (class ReadForeign, readJSON)

infixr 9 compose as ∘

infixr 5 append as ⫲

infixl 7 mul as ×

infixl 7 div as ÷

infixl 4 lessThanOrEq as ≤

infixl 4 greaterThanOrEq as ≥

infix 4 notEq as ≠

infixr 2 disj as ∨

infixr 3 conj as ∧

infixl 4 nothingCoalesce as ∥

infixl 8 index as ↸

infixr 5 cons as ▶

infixl 7 mod as %

infix 5 difference as ∖

nothingCoalesce ∷ ∀ a. Maybe a → a → a
nothingCoalesce = flip fromMaybe

type JSONResult a
  = Either MultipleErrors a

loadJSON ∷ ∀ a. ReadForeign a ⇒ String → Effect (JSONResult a)
loadJSON path = do
  rawJSON <- readTextFile UTF8 path
  pure $ readJSON rawJSON

cartesianProduct2 ∷ ∀ f g a b. Foldable f ⇒ Foldable g ⇒ f a → g b → Array (Tuple a b)
cartesianProduct2 as bs = do
  a ← fromFoldable as
  b ← fromFoldable bs
  pure $ Tuple a b

id ∷ ∀ a. a → a
id a = a
