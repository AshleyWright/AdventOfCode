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

data Computer
  = Computer Program Int Boolean

instance showComputer ∷ Show Computer where
  show (Computer prog count halt) = "(Computer " ⫲ (show prog) ⫲ " " ⫲ (show count) ⫲ " " ⫲ (show halt) ⫲ ")"

derive instance eqComputer ∷ Eq Computer

fetch ∷ Address → Computer → Int
fetch (Address a) (Computer prog _ _) = prog ↸ a ∥ 0

indirect ∷ Address → Computer → Int
indirect a comp = fetch (wrap $ fetch a comp) comp

decode ∷ OpCode → Address → Int → Int → Computer → Computer
decode (OpCode 1) = execute (+)

decode (OpCode 2) = execute (×)

decode (OpCode 99) = \_ _ _ (Computer p c _) → Computer p c true

decode _ = \_ _ _ → id

write ∷ Int → Address → Computer → Computer
write val (Address a) (Computer prog count halt) = Computer (updateAt a val prog ∥ prog) count halt

execute ∷ (Int → Int → Int) → Address → Int → Int → Computer → Computer
execute f res op1 op2 = write (f op1 op2) res

step ∷ Computer → Computer
step (Computer prog count halt) = Computer prog (count + 4) halt

hasHalted ∷ Computer → Boolean
hasHalted (Computer _ _ halt) = halt

getCurrentInstructionAddress ∷ Computer → Address
getCurrentInstructionAddress (Computer _ count _) = wrap count

offset ∷ Int → Address → Address
offset o (Address a) = wrap (a + o)

tick ∷ Computer → Computer
tick computer@(Computer prog count _) = decode opCode resultAddress op1 op2 computer
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
run prog = case (run' $ Computer prog 0 false) of
  (Computer prog' _ _) → prog'
  where
  run' comp
    | hasHalted comp = comp
    | otherwise = (run' ∘ step ∘ tick) comp

main ∷ Effect Unit
main = do
  json ∷ JSONResult Program ← loadJSON "./data/02.json"
  case json of
    Right input → do
      logShow ∘ run $ (updateAt 2 2 $ updateAt 1 12 input ∥ input) ∥ input
    Left e → do
      logShow e
