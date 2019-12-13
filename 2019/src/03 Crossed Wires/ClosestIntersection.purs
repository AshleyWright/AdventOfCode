module ClosestIntersection where

import Prelude (class Eq, class Ord, class Semiring, class Show, class Ring, Unit, between, bind, map, otherwise, pure, ($), (+), (-), (<$>), (==))
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
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Common (JSONResult, loadJSON, (∘), (≠), (≤), (×), (÷), (∧), (∥), (⫲), (↸))

type Coord a r
  = { x ∷ a, y ∷ a | r }

type Vector
  = Tuple String Int

type Line a r s
  = { from ∷ Coord a r, to ∷ Coord a r | s }

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

makeLine ∷ Coord Int () → Vector → Line Int () ()
makeLine cursor vector =
  { from: cursor
  , to:
    case vector of
      Tuple "U" δy → cursor { y = cursor.y + δy }
      Tuple "R" δx → cursor { x = cursor.x + δx }
      Tuple "D" δy → cursor { y = cursor.y - δy }
      Tuple "L" δx → cursor { x = cursor.x - δx }
      _ → cursor { x = 0, y = 0 }
  }

manhattenDistance ∷ ∀ a r. Semiring a ⇒ Ord a ⇒ Ring a ⇒ Coord a r → a
manhattenDistance coord = abs coord.x + abs coord.y

orientation ∷ ∀ r s. Line Int r s → Orientation
orientation { from: start, to: end }
  | end.x - start.x == 0 = Vertical
  | end.y - start.y == 0 = Horizontal
  | otherwise = Diagonal

gradient ∷ ∀ r s. Line Int r s → Number
gradient { from: start, to: end } = Int.toNumber (end.y - start.y) ÷ Int.toNumber (end.x - start.x)

intersect ∷ ∀ r s. Line Int r s → Number
intersect l@{ from: { x: x, y: y } } = Int.toNumber y - gradient l × Int.toNumber x

areParrallel ∷ ∀ r s t u. Line Int r s → Line Int t u → Boolean
areParrallel ℓ0 ℓ1
  | orientation ℓ0 == orientation ℓ1 ∧ orientation ℓ0 ≠ Diagonal = true
  | gradient ℓ0 == gradient ℓ1 = true
  | otherwise = false

between' ∷ ∀ a. Ord a ⇒ a → a → a → Boolean
between' min max = if min ≤ max then between min max else between max min

intersection ∷ ∀ r s t u. Line Int r s → Line Int t u → Maybe (Coord Int ())
intersection ℓ0 ℓ1
  | areParrallel ℓ0 ℓ1 = Nothing
  | orientation ℓ0 == Vertical =
    let
      y ∷ Int
      y = Int.floor $ gradient ℓ1 × Int.toNumber ℓ0.from.x + intersect ℓ1
    in
      if between' ℓ1.from.x ℓ1.to.x ℓ0.from.x ∧ between' ℓ0.from.y ℓ0.to.y y ∧ between' ℓ1.from.y ℓ1.to.y y then
        Just { x: ℓ0.from.x, y: y }
      else
        Nothing
  | orientation ℓ1 == Vertical =
    let
      y ∷ Int
      y = Int.floor $ gradient ℓ0 × Int.toNumber ℓ1.from.x + intersect ℓ0
    in
      if between' ℓ0.from.x ℓ0.to.x ℓ1.from.x ∧ between' ℓ0.from.y ℓ0.to.y y ∧ between' ℓ1.from.y ℓ1.to.y y then
        Just { x: ℓ1.from.x, y: y }
      else
        Nothing
  | otherwise =
    let
      x ∷ Int
      x = Int.floor $ (intersect ℓ1 - intersect ℓ0) ÷ (gradient ℓ0 - gradient ℓ1)

      y ∷ Int
      y = Int.floor $ gradient ℓ0 × Int.toNumber x + intersect ℓ0
    in
      if between' ℓ0.from.x ℓ0.to.x x ∧ between' ℓ1.from.x ℓ1.to.x x ∧ between' ℓ0.from.y ℓ0.to.y y ∧ between' ℓ1.from.y ℓ1.to.y y then
        Just { x: x, y: y }
      else
        Nothing

minByMaybe ∷ ∀ a b. Ord b ⇒ (a → b) → Maybe a → Maybe a → Maybe a
minByMaybe f x y
  | isNothing x = y
  | isNothing y = x
  | f (unsafePartial $ fromJust x) ≤ f (unsafePartial $ fromJust y) = x
  | otherwise = y

makeLines ∷ Array String → Array (Line Int () ())
makeLines = makeLines' [] { x: 0, y: 0 }
  where
  makeLines' acc cursor moves
    | length moves ≤ 0 = acc
    | otherwise =
      let
        line = makeLine cursor $ toVector $ unsafePartial $ fromJust $ head moves
      in
        makeLines' (acc ⫲ [ line ]) (if line.from == cursor then line.to else line.from) (tail moves ∥ [])

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array (Array String)) ← loadJSON "./data/03.json"
  case json of
    Right input →
      logShow $ manhattenDistance $ fromMaybe { x: 0, y: 0 } $ foldl1 (minByMaybe manhattenDistance) $ NonEmpty Nothing
        $ filter isJust do
            line1 ← makeLines (input ↸ 0 ∥ [])
            line2 ← makeLines (input ↸ 1 ∥ [])
            pure $ intersection line1 line2
    Left e → do
      logShow e
