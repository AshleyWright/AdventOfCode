module IntcodeComputer where

import Prelude (class Eq, class Show, Unit, bind, otherwise, show, ($), (+))
import Data.Array (updateAt)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Console (logShow)
import Common (JSONResult, id, loadJSON, (∘), (⫲), (∥), (↸), (×))

type Program
  = Array Int

newtype Address
  = Address Int

instance showAddress ∷ Show Address where
  show address = "(Address " ⫲ (show ∘ unwrap) address ⫲ ")"

derive instance newtypeAddress ∷ Newtype Address _

derive instance eqAddress ∷ Eq Address

newtype OpCode
  = OpCode Int

instance showOpCode ∷ Show OpCode where
  show opCode = "(OpCode " ⫲ (show ∘ unwrap) opCode ⫲ ")"

derive instance newtypeOpCode ∷ Newtype OpCode _

derive instance eqOpCode ∷ Eq OpCode

type Computer
  = { memory ∷ Program
    , instructionPointer ∷ Int
    , halted ∷ Boolean
    }

fetch ∷ Address → Computer → Int
fetch (Address a) { memory } = memory ↸ a ∥ 0

indirect ∷ Address → Computer → Int
indirect a comp = fetch (wrap $ fetch a comp) comp

decode ∷ OpCode → Address → Int → Int → Computer → Computer
decode (OpCode 1) = execute (+)

decode (OpCode 2) = execute (×)

decode (OpCode 99) = \_ _ _ comp → comp { halted = true }

decode _ = \_ _ _ → id

write ∷ Int → Address → Computer → Computer
write val (Address a) comp@{ memory } = comp { memory = (updateAt a val memory ∥ memory) }

execute ∷ (Int → Int → Int) → Address → Int → Int → Computer → Computer
execute f res op1 op2 = write (f op1 op2) res

step ∷ Computer → Computer
step comp@{ instructionPointer } = comp { instructionPointer = instructionPointer + 4 }

getCurrentInstructionAddress ∷ Computer → Address
getCurrentInstructionAddress = wrap ∘ (_.instructionPointer)

offset ∷ Int → Address → Address
offset o (Address a) = wrap (a + o)

tick ∷ Computer → Computer
tick computer = decode opCode resultAddress op1 op2 computer
  where
  opCode ∷ OpCode
  opCode = wrap $ fetch instructionAddress computer

  instructionAddress ∷ Address
  instructionAddress = getCurrentInstructionAddress computer

  op1 ∷ Int
  op1 = indirect (offset 1 instructionAddress) computer

  op2 ∷ Int
  op2 = indirect (offset 2 instructionAddress) computer

  resultAddress ∷ Address
  resultAddress = wrap $ fetch (offset 3 instructionAddress) computer

run ∷ Program → Program
run prog = (_.memory) (run' { memory: prog, instructionPointer: 0, halted: false })
  where
  run' comp
    | comp.halted = comp
    | otherwise = (run' ∘ step ∘ tick) comp

main ∷ Effect Unit
main = do
  json ∷ JSONResult Program ← loadJSON "./data/02.json"
  case json of
    Right input → do
      logShow ∘ run $ (updateAt 2 2 $ updateAt 1 12 input ∥ input) ∥ input
    Left e → do
      logShow e
