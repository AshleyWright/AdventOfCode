module CalculateFuelRequirementRecursive where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (log, logShow)
import Common
import CalculateFuelRequirement (getRequiredFuel)

getRequiredFuelRecursive ∷ Number → Number
getRequiredFuelRecursive mass
  | getRequiredFuel mass <= 0.0 = 0.0
  | otherwise = fuelMass + getRequiredFuelRecursive fuelMass
    where
    fuelMass = getRequiredFuel mass

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array Number) ← loadJSON "./data/01.json"
  case json of
    Right input → do
      logShow $ foldl (+) 0.0 $ map getRequiredFuelRecursive input
    Left e → do
      logShow e
