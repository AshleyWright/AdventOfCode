module IntcodeComputer where

import Prelude

import Data.Array (updateAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Console (log, logShow)

import Common

type Program = Array Int
newtype Address = Address Int
newtype OpCode = OpCode Int
data Computer = Computer Program Int Boolean

fetch ∷ Address → Computer → Int
fetch (Address a) (Computer prog _ _) = fromMaybe 0 $ prog !! a

indirect ∷ Address → Computer → Int
indirect a comp = fetch (Address $ fetch a comp) comp

decode ∷ OpCode → Address → Int → Int → Computer → Computer
decode (OpCode  1) = execute (+)
decode (OpCode  2) = execute (*)
decode (OpCode 99) = (\_ _ _ (Computer p c _) → Computer p c true)
decode  _          = (\_ _ _  comp            → comp)

write ∷ Int → Address → Computer → Computer
write val (Address a) (Computer prog count halt) = Computer (fromMaybe prog $ updateAt a val prog) count halt

execute ∷ (Int → Int → Int) → Address → Int → Int → Computer → Computer
execute f res op1 op2 = write (f op1 op2) res

step ∷ Computer → Computer
step (Computer prog count halt) = Computer prog (count + 4) halt

hasHalted ∷ Computer → Boolean
hasHalted (Computer _ _ halt) = halt

getCurrentInstructionAddress ∷ Computer → Address
getCurrentInstructionAddress (Computer _ count _) = Address count

offset ∷ Int → Address → Address
offset o (Address a) = Address (a + o)

tick ∷ Computer → Computer
tick computer@(Computer prog count _) = trace (show [[count], prog]) \_ -> decode opCode resultAddress op1 op2 computer
  where
    opCode             = OpCode $ fetch instructionAddress computer
    instructionAddress = getCurrentInstructionAddress computer
    op1                = indirect (offset 1 instructionAddress) computer
    op2                = indirect (offset 2 instructionAddress) computer
    resultAddress      = Address $ fetch (offset 3 instructionAddress) computer

run ∷ Program → Program
run prog = case (run' $ Computer prog 0 false) of
  (Computer prog _ _) → prog
  where
    run' comp | hasHalted comp = comp
              | otherwise      = run' $ step (tick comp)

main ∷ Effect Unit
main = do
  json ∷ JSONResult (Array Int) ← loadJSON "./data/02.json"
  case json of
    Right input → do
      logShow $ run (fromMaybe input $ updateAt 2 2 (fromMaybe input $ updateAt 1 12 input))
    Left e → do
      logShow e