module Common where

import Prelude
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable
import Data.Tuple
import Effect
import Foreign (MultipleErrors)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (readJSON)

type JSONResult a
  = Either MultipleErrors a

loadJSON path = do
  rawJSON <- readTextFile UTF8 path
  pure $ readJSON rawJSON

cartesianProduct2 ∷ ∀ f g a b. Foldable f ⇒ Foldable g ⇒ f a → g b → Array (Tuple a b)
cartesianProduct2 as bs = do
  a ← fromFoldable as
  b ← fromFoldable bs
  pure $ Tuple a b
