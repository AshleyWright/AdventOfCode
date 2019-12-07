module Common where

import Prelude (bind, pure, ($))
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Tuple (Tuple(..))
import Effect
import Foreign (MultipleErrors)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (class ReadForeign, readJSON)

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
