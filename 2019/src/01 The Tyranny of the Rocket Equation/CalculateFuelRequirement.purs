module CalculateFuelRequirement where

import Prelude (Unit, bind, map, ($), (+), (-))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow)
import Math (floor)
import Common (JSONResult, loadJSON, (÷))

getRequiredFuel ∷ Number → Number
getRequiredFuel mass = floor (mass ÷ 3.0) - 2.0

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array Number) ← loadJSON "./data/01.json"
  case json of
    Right input → do
      logShow $ foldl (+) 0.0 $ map getRequiredFuel input
    Left e → do
      logShow e
