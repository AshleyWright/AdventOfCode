module GravityAssist where

import Prelude
import Data.Array (filter, head, length, range, tail, updateAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Common
import IntcodeComputer

init ∷ Int → Int → Program → Program
init noun verb prog = fromMaybe prog $ updateAt 2 verb (fromMaybe prog $ updateAt 1 noun prog)

getOutput ∷ Program → Int
getOutput prog = fromMaybe 0 $ prog !! 0

findInputs ∷ Program → Tuple Int Int → Tuple Int Int → Int → Maybe (Tuple Int Int)
findInputs prog (Tuple nmin nmax) (Tuple vmin vmax) target = findInputs' $ cartesianProduct2 nouns verbs
  where
  nouns = range nmin nmax

  verbs = range vmin vmax

  findInputs' ∷ Array (Tuple Int Int) → Maybe (Tuple Int Int)
  findInputs' candidates
    | length candidates == 0 = Nothing
    | testInputs $ unsafePartial $ fromJust $ head candidates = head candidates
    | otherwise = findInputs' $ fromMaybe [] $ tail candidates

  testInputs ∷ Tuple Int Int → Boolean
  testInputs (Tuple n v) = (getOutput $ run $ init n v prog) == target

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array Int) ← loadJSON "./data/02.json"
  case json of
    Right input → do
      logShow $ findInputs input (Tuple 0 99) (Tuple 0 99) 19690720
    Left e → do
      logShow e
