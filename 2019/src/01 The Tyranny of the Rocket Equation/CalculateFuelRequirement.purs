module CalculateFuelRequirement where

import Prelude

import Data.Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Effect.Console (log, logShow)

import Common

main = do
  json ∷ JSONResult (Array Int) <- loadJSON "./data/01.json"
  case json of
    Right input -> do
      logShow $ foldl (+) 0 $ map (\x -> x / 3 - 2) input
    Left e -> do
      logShow e