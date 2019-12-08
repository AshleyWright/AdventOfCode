module GravityAssist where

import Prelude (Unit, bind, otherwise, ($), (==))
import Data.Array (head, length, range, tail, updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Common (JSONResult, cartesianProduct2, loadJSON, (∥), (↸), (∘))
import IntcodeComputer (Program, run)

init ∷ Int → Int → Program → Program
init noun verb prog = updateAt 2 verb (updateAt 1 noun prog ∥ prog) ∥ prog

getOutput ∷ Program → Int
getOutput prog = head prog ∥ 0

findInputs ∷ Program → Tuple Int Int → Tuple Int Int → Int → Maybe (Tuple Int Int)
findInputs prog (Tuple nmin nmax) (Tuple vmin vmax) target = findInputs' $ cartesianProduct2 nouns verbs
  where
  nouns ∷ Array Int
  nouns = range nmin nmax

  verbs ∷ Array Int
  verbs = range vmin vmax

  findInputs' ∷ Array (Tuple Int Int) → Maybe (Tuple Int Int)
  findInputs' candidates
    | length candidates == 0 = Nothing
    | testNext candidates    = head candidates
    | otherwise              = findInputs' $ tail candidates ∥ []

  testNext ∷ Array (Tuple Int Int) → Boolean
  testNext candidates = testInputs $ unsafePartial $ fromJust ∘ head $ candidates

  testInputs ∷ Tuple Int Int → Boolean
  testInputs (Tuple n v) = (getOutput ∘ run $ init n v prog) == target

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array Int) ← loadJSON "./data/02.json"
  case json of
    Right input → do
      logShow $ findInputs input (Tuple 0 99) (Tuple 0 99) 19690720
    Left e → do
      logShow e
