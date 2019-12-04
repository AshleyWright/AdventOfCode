module Common where

import Prelude
import Data.Either (Either(..))
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
