module MinimiseSignalDelay where

import Prelude (Unit, bind, pure, zero, ($), (+), (-))
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.NonEmpty (NonEmpty(..), foldl1)
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import ClosestIntersection (Coord, Line, intersection, makeLines, manhattenDistance, minByMaybe)
import Common (JSONResult, loadJSON, (∥), (↸))

signalDelayAtIntersection ∷ { line1 ∷ Line Int ( signalDelay ∷ Int ) (), line2 ∷ Line Int ( signalDelay ∷ Int ) (), intersection ∷ Coord Int () } → Int
signalDelayAtIntersection { line1, line2, intersection } = line1.from.signalDelay + line2.from.signalDelay + manhattenDistance δ1 + manhattenDistance δ2
  where
  δ1 = { x: intersection.x - line1.from.x, y: intersection.y - line1.from.y }

  δ2 = { x: intersection.x - line2.from.x, y: intersection.y - line2.from.y }

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array (Array String)) ← loadJSON "./data/03.json"
  case json of
    Right input →
      logShow $ signalDelayAtIntersection $ fromMaybe (zero ∷ { line1 ∷ Line Int ( signalDelay ∷ Int ) (), line2 ∷ Line Int ( signalDelay ∷ Int ) (), intersection ∷ Coord Int () }) $ foldl1 (minByMaybe signalDelayAtIntersection) $ NonEmpty Nothing
        $ filter isJust do
            line1 ← makeLines (input ↸ 0 ∥ [])
            line2 ← makeLines (input ↸ 1 ∥ [])
            pure
              $ if isJust (intersection line1 line2) then
                  Just { line1: line1, line2: line2, intersection: unsafePartial (fromJust $ intersection line1 line2) }
                else
                  Nothing
    Left e → do
      logShow e
