module ClosestIntersection where

import Prelude (class Eq, class Ord, class Show, Unit, between, bind, map, otherwise, pure, ($), (+), (-), (<$>), (==))
import Data.Array (filter, fromFoldable, head, last, length, tail)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing)
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.Ord (abs)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Common (JSONResult, loadJSON, (∘), (≠), (≤), (×), (÷), (∧), (∥), (⫲), (↸))

type Coord
  = Tuple Int Int

type Vector
  = Tuple String Int

type Line
  = Tuple Coord Coord

data Orientation
  = Horizontal
  | Vertical
  | Diagonal

derive instance eqOrientation ∷ Eq Orientation

instance showOrientation ∷ Show Orientation where
  show Horizontal = "Horizontal"
  show Vertical = "Vertical"
  show Diagonal = "Diagonal"

toVector ∷ String → Vector
toVector = toVector' ∘ tail' ∘ match' (unsafePartial $ fromRight $ regex """^([A-Z]*)(\d*)$""" noFlags)
  where
  tail' ∷ ∀ a f. Foldable f ⇒ f a → Array a
  tail' = unsafePartial $ fromJust ∘ tail ∘ fromFoldable

  match' ∷ Regex → String → NonEmptyArray String
  match' = unsafePartial \pattern → map fromJust ∘ fromJust ∘ match pattern

  toVector' ∷ Array String → Vector
  toVector' array = Tuple (unsafePartial $ fromJust $ head array) (unsafePartial $ fromJust $ (fromJust ∘ Int.fromString) <$> last array)

makeLine ∷ Coord → Vector → Line
makeLine from@(Tuple x y) vector = if from ≤ to then Tuple from to else Tuple to from
  where
  to ∷ Coord
  to = case vector of
    Tuple "U" δy → Tuple x (y + δy)
    Tuple "R" δx → Tuple (x + δx) y
    Tuple "D" δy → Tuple x (y - δy)
    Tuple "L" δx → Tuple (x - δx) y
    _ → Tuple 0 0

manhattenDistance ∷ Coord → Int
manhattenDistance (Tuple x y) = abs x + abs y

orientation ∷ Line → Orientation
orientation l@(Tuple (Tuple x0 y0) (Tuple x1 y1))
  | x1 - x0 == 0 = Vertical
  | y1 - y0 == 0 = Horizontal
  | otherwise = Diagonal

gradient ∷ Line → Number
gradient (Tuple (Tuple x0 y0) (Tuple x1 y1)) = Int.toNumber (y1 - y0) ÷ Int.toNumber (x1 - x0)

intersect ∷ Line → Number
intersect l@(Tuple (Tuple x y) _) = Int.toNumber y - gradient l × Int.toNumber x

areParrallel ∷ Line → Line → Boolean
areParrallel ℓ0 ℓ1
  | orientation ℓ0 == orientation ℓ1 ∧ orientation ℓ0 ≠ Diagonal = true
  | gradient ℓ0 == gradient ℓ1 = true
  | otherwise = false

between' ∷ ∀ a. Ord a ⇒ a → a → a → Boolean
between' min max = if min ≤ max then between min max else between max min

intersection ∷ Line → Line → Maybe Coord
intersection ℓ0@(Tuple (Tuple x00 y00) (Tuple x01 y01)) ℓ1@(Tuple (Tuple x10 y10) (Tuple x11 y11))
  | areParrallel ℓ0 ℓ1 = Nothing
  | orientation ℓ0 == Vertical =
    let
      y ∷ Int
      y = Int.floor $ gradient ℓ1 × Int.toNumber x00 + intersect ℓ1
    in
      if between' x10 x11 x00 ∧ between' y00 y01 y ∧ between' y10 y11 y then
        Just (Tuple x00 y)
      else
        Nothing
  | orientation ℓ1 == Vertical =
    let
      y ∷ Int
      y = Int.floor $ gradient ℓ0 × Int.toNumber x10 + intersect ℓ0
    in
      if between' x00 x01 x10 ∧ between' y00 y01 y ∧ between' y10 y11 y then
        Just (Tuple x10 y)
      else
        Nothing
  | otherwise =
    let
      x ∷ Int
      x = Int.floor $ (intersect ℓ1 - intersect ℓ0) ÷ (gradient ℓ0 - gradient ℓ1)

      y ∷ Int
      y = Int.floor $ gradient ℓ0 × Int.toNumber x + intersect ℓ0
    in
      if between' x00 x01 x ∧ between' x10 x11 x ∧ between' y00 y01 y ∧ between' y10 y11 y then
        Just (Tuple x y)
      else
        Nothing

minByMaybe ∷ ∀ a b. Ord b ⇒ (a → b) → Maybe a → Maybe a → Maybe a
minByMaybe f x y
  | isNothing x = y
  | isNothing y = x
  | f (unsafePartial $ fromJust x) ≤ f (unsafePartial $ fromJust y) = x
  | otherwise = y

makeLines ∷ Array String → Array Line
makeLines = makeLines' [] (Tuple 0 0)
  where
  makeLines' ∷ Array Line → Coord → Array String → Array Line
  makeLines' acc cursor moves
    | length moves ≤ 0 = acc
    | otherwise =
      let
        line ∷ Line
        line = makeLine cursor $ toVector $ unsafePartial $ fromJust $ head moves
      in
        makeLines' (acc ⫲ [ line ]) (if fst line == cursor then snd line else fst line) (tail moves ∥ [])

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array (Array String)) ← loadJSON "./data/03.json"
  case json of
    Right input →
      logShow $ manhattenDistance $ fromMaybe (Tuple 0 0) $ foldl1 (minByMaybe manhattenDistance) $ NonEmpty Nothing
        $ filter isJust do
            line1 ← makeLines (input ↸ 0 ∥ [])
            line2 ← makeLines (input ↸ 1 ∥ [])
            pure $ intersection line1 line2
    Left e → do
      logShow e
