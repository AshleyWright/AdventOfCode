module ThermalRadiators where

import Prelude (class Eq, class Show, Unit, bind, discard, map, pure, ($), (+), (==), (<))
import Data.Array (fromFoldable, head, slice, tail, updateAt, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num.Reps (D4)
import Data.Vec (Vec, empty, zipWith)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Process (exit)
import IntcodeComputer (Address(..), Program)
import Common (JSONResult, loadJSON, (%), (∘), (▶), (↸), (∥), (×), (÷), (≠))

data ParameterMode
  = Position
  | Immediate
  | UnknownParameterMode

instance showParameterMode ∷ Show ParameterMode where
  show ∷ ParameterMode → String
  show Position = "Position"
  show Immediate = "Immediate"
  show UnknownParameterMode = "UnknownParameterMode"

derive instance eqParameterMode ∷ Eq ParameterMode

data AccessMode
  = Read
  | Write
  | NoAccess

instance showAccessMode ∷ Show AccessMode where
  show ∷ AccessMode → String
  show Read = "Read"
  show Write = "Write"
  show NoAccess = "NoAccess"

derive instance eqAccessMode ∷ Eq AccessMode

data Operation
  = Add
  | Multiply
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | Halt
  | UnknownOperation

instance showOperation ∷ Show Operation where
  show ∷ Operation → String
  show Add = "Add"
  show Multiply = "Multiply"
  show Input = "Input"
  show Output = "Output"
  show JumpIfTrue = "JumpIfTrue"
  show JumpIfFalse = "JumpIfFalse"
  show LessThan = "LessThan"
  show Equals = "Equals"
  show Halt = "Halt"
  show UnknownOperation = "UnknownOperation"

derive instance eqOperation ∷ Eq Operation

type Computer
  = { memory ∷ Program
    , register ∷ Array Int
    , inputQueue ∷ Array Int
    , instructionPointer ∷ Address
    , currentOperation ∷ Operation
    , parameterModes ∷ Vec D4 ParameterMode
    , halted ∷ Boolean
    , hasJumped ∷ Boolean
    }

parameterCount ∷ Operation → Int
parameterCount Add = 3

parameterCount Multiply = 3

parameterCount Input = 1

parameterCount Output = 1

parameterCount JumpIfTrue = 2

parameterCount JumpIfFalse = 2

parameterCount LessThan = 3

parameterCount Equals = 3

parameterCount Halt = 0

parameterCount UnknownOperation = 0

step ∷ Computer → Computer
step comp@{ instructionPointer, currentOperation, hasJumped } =
  if hasJumped then
    comp { hasJumped = false }
  else
    comp
      { instructionPointer =
        wrap $ (unwrap instructionPointer) + parameterCount currentOperation + 1
      }

getOperation ∷ Int → Operation
getOperation x = case (x % 100) of
  1 → Add
  2 → Multiply
  3 → Input
  4 → Output
  5 → JumpIfTrue
  6 → JumpIfFalse
  7 → LessThan
  8 → Equals
  99 → Halt
  _ → UnknownOperation

getAccessModes ∷ Operation → Vec D4 AccessMode
getAccessModes Add = Read ▶ Read ▶ Write ▶ NoAccess ▶ empty

getAccessModes Multiply = Read ▶ Read ▶ Write ▶ NoAccess ▶ empty

getAccessModes Input = Write ▶ NoAccess ▶ NoAccess ▶ NoAccess ▶ empty

getAccessModes Output = Read ▶ NoAccess ▶ NoAccess ▶ NoAccess ▶ empty

getAccessModes JumpIfTrue = Read ▶ Read ▶ NoAccess ▶ NoAccess ▶ empty

getAccessModes JumpIfFalse = Read ▶ Read ▶ NoAccess ▶ NoAccess ▶ empty

getAccessModes LessThan = Read ▶ Read ▶ Write ▶ NoAccess ▶ empty

getAccessModes Equals = Read ▶ Read ▶ Write ▶ NoAccess ▶ empty

getAccessModes Halt = NoAccess ▶ NoAccess ▶ NoAccess ▶ NoAccess ▶ empty

getAccessModes UnknownOperation = NoAccess ▶ NoAccess ▶ NoAccess ▶ NoAccess ▶ empty

getParameterModes ∷ Int → Vec D4 ParameterMode
getParameterModes x = zipWith (\a p → if a == Write then Immediate else p) (getAccessModes ∘ getOperation $ x) (pm ((x ÷ 100) % 10) ▶ pm ((x ÷ 1000) % 10) ▶ pm ((x ÷ 10000) % 10) ▶ pm ((x ÷ 100000) % 10) ▶ empty)
  where
  pm ∷ Int → ParameterMode
  pm 0 = Position

  pm 1 = Immediate

  pm _ = UnknownParameterMode

decode ∷ Computer → Computer
decode comp@{ instructionPointer, memory } = case (memory ↸ unwrap instructionPointer) of
  Nothing →
    comp
      { currentOperation = UnknownOperation
      , parameterModes = UnknownParameterMode ▶ UnknownParameterMode ▶ UnknownParameterMode ▶ UnknownParameterMode ▶ empty
      }
  Just instruction →
    comp
      { currentOperation = getOperation instruction
      , parameterModes = getParameterModes instruction
      }

fetch ∷ Computer → Computer
fetch comp@{ instructionPointer, currentOperation, memory, parameterModes } =
  comp
    { register =
      map fetch' ∘ zip (fromFoldable parameterModes) $ slice (unwrap instructionPointer + 1) (unwrap instructionPointer + parameterCount currentOperation + 1) memory
    }
  where
  fetch' ∷ Tuple ParameterMode Int → Int
  fetch' (Tuple Position index) = memory ↸ index ∥ 0

  fetch' (Tuple Immediate value) = value

  fetch' _ = 0

execute ∷ Computer → Effect Computer
execute comp@{ currentOperation, inputQueue, memory, register } = case currentOperation of
  Add → write (register ↸ 2) $ pure $ (register ↸ 0 ∥ 0) + (register ↸ 1 ∥ 0)
  Multiply → write (register ↸ 2) $ pure $ (register ↸ 0 ∥ 0) × (register ↸ 1 ∥ 0)
  Input → do
    comp' ← write (register ↸ 0) $ pure (head inputQueue ∥ 0)
    pure $ comp' { inputQueue = tail inputQueue ∥ [] }
  Output → do
    logShow (register ↸ 0 ∥ 0)
    pure comp
  JumpIfTrue →
    if (register ↸ 0 ∥ 0) ≠ 0 then
      pure $ comp { instructionPointer = wrap (register ↸ 1 ∥ 0), hasJumped = true }
    else
      pure $ comp
  JumpIfFalse →
    if (register ↸ 0 ∥ 0) == 0 then
      pure $ comp { instructionPointer = wrap (register ↸ 1 ∥ 0), hasJumped = true }
    else
      pure $ comp
  LessThan → write (register ↸ 2) $ pure $ if (register ↸ 0 ∥ 0) < (register ↸ 1 ∥ 0) then 1 else 0
  Equals → write (register ↸ 2) $ pure $ if (register ↸ 0 ∥ 0) == (register ↸ 1 ∥ 0) then 1 else 0
  Halt → pure $ comp { halted = true }
  UnknownOperation → do
    _ ← exit 1
    pure comp
  where
  write (Just address) m = do
    value ← m
    pure $ comp { memory = (updateAt address value memory ∥ memory) }

  write Nothing _ = pure comp

tick ∷ Computer → Effect Computer
tick comp = do
  comp' ← execute ∘ fetch ∘ decode $ comp
  pure $ step comp'

run ∷ Program → Array Int → Effect Program
run prog inputs = do
  endState ←
    run'
      { memory: prog
      , register: []
      , inputQueue: inputs
      , instructionPointer: Address 0
      , currentOperation: UnknownOperation
      , parameterModes: UnknownParameterMode ▶ UnknownParameterMode ▶ UnknownParameterMode ▶ UnknownParameterMode ▶ empty
      , halted: false
      , hasJumped: false
      }
  pure endState.memory
  where
  run' ∷ Computer → Effect Computer
  run' comp = do
    comp' ← tick comp
    if comp'.halted then pure comp' else run' comp'

main ∷ Effect Unit
main = do
  json ∷ JSONResult Program ← loadJSON "./data/05.json"
  case json of
    Right prog → do
      endState ← run prog [ 5 ]
      logShow endState
    Left e → do
      logShow e
