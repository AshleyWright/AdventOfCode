module OrbitCountChecksum where

import Prelude (Unit, bind, map, ($), (+), (==))
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, find, foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Common (JSONResult, loadJSON, (∥), (↸))

count ∷ ∀ f. Foldable f ⇒ f (Array String) → Array String → Int
count orbits orb =
  1
    + case find (\o → (o ↸ 1 ∥ "NULL") == (head orb ∥ "NULL")) orbits of
        Nothing → 0
        Just c → count orbits c

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array (Array String)) ← loadJSON "./data/06.json"
  case json of
    Right orbits → do
      logShow $ foldl (+) 0 $ map (count orbits) orbits
    Left e → do
      logShow e
