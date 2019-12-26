module OrbitalTransfers where

import Prelude (Unit, bind, discard, map, pure, ($), (-), (==))
import Data.Array (filter, head, last, length, tail)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, find)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing)
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Common (JSONResult, loadJSON, (∥), (⫲), (∘), (∖))

getCentroid ∷ ∀ f. Foldable f ⇒ f (Array String) → String → Maybe String
getCentroid orbits satellite = do
  maybeOrbit ← find (\orbit → (last orbit ∥ "NULL") == satellite) orbits
  pure $ head maybeOrbit ∥ "NULL"

getSattellites ∷ Array (Array String) → String → Array String
getSattellites orbits centroid = map (fromMaybe "NULL" ∘ last) ∘ filter (\orbit → (head orbit ∥ "NULL") == centroid) $ orbits

getTransferAdj ∷ Array (Array String) → String → Array String
getTransferAdj orbits node = let centroid = getCentroid orbits node in getSattellites orbits node ⫲ if isJust centroid then [ unsafePartial $ fromJust centroid ] else []

path ∷ Array (Array String) → String → String → Array String
path orbits from to = path' [] [ from ] to
  where
  path' ∷ Array (Array String) → Array String → String → Array String
  path' frontier cur target =
    if last cur == Just target then
      cur
    else
      let
        newFront = frontier ⫲ map (\n → cur ⫲ [ n ]) (getTransferAdj orbits (last cur ∥ "NULL") ∖ cur)

        next = head newFront
      in
        if isNothing next then
          []
        else
          path' (tail newFront ∥ []) (unsafePartial $ fromJust next) target

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array (Array String)) ← loadJSON "./data/06.json"
  case json of
    Right orbits → do
      let
        p = path orbits "YOU" "SAN"
      logShow p
      logShow $ length p - 3
    Left e → do
      logShow e
