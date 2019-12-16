module Test.Day02 where

import Prelude
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Test.Unit as UnitTest
import Test.Unit.Assert as Assert
import IntcodeComputer (Address(..), OpCode(..), decode, execute, fetch, getCurrentInstructionAddress, indirect, offset, run, step, tick, write)
import GravityAssist (findInputs, getOutput, init)
import Common ((×))

test ∷ UnitTest.TestSuite
test =
  UnitTest.suite "02 1202 Program Alarm" do
    UnitTest.test "fetch" do
      Assert.equal 1 $ fetch (Address 0) { memory: [ 1 ], instructionPointer: 0, halted: false }
      Assert.equal 1 $ fetch (Address 0) { memory: [ 1 ], instructionPointer: 0, halted: true }
      Assert.equal 0 $ fetch (Address 0) { memory: [], instructionPointer: 0, halted: false }
      Assert.equal 0 $ fetch (Address 0) { memory: [], instructionPointer: 0, halted: true }
    UnitTest.test "indirect" do
      Assert.equal 2 $ indirect (Address 0) { memory: [ 1, 2 ], instructionPointer: 0, halted: false }
      Assert.equal 2 $ indirect (Address 0) { memory: [ 1, 2 ], instructionPointer: 0, halted: true }
      Assert.equal 1 $ indirect (Address 1) { memory: [ 0, 1 ], instructionPointer: 0, halted: false }
      Assert.equal 1 $ indirect (Address 1) { memory: [ 0, 1 ], instructionPointer: 0, halted: true }
      Assert.equal 0 $ indirect (Address 0) { memory: [ 2, 1 ], instructionPointer: 0, halted: false }
      Assert.equal 0 $ indirect (Address 0) { memory: [ 2, 1 ], instructionPointer: 0, halted: true }
    UnitTest.test "decode" do
      Assert.equal { memory: [ 2 ], instructionPointer: 0, halted: false } $ decode (OpCode 1) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 2 ], instructionPointer: 0, halted: true } $ decode (OpCode 1) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: true }
      Assert.equal { memory: [ 0, 2 ], instructionPointer: 0, halted: true } $ decode (OpCode 1) (Address 1) 1 1 { memory: [ 0, 1 ], instructionPointer: 0, halted: true }
      Assert.equal { memory: [ 1 ], instructionPointer: 0, halted: false } $ decode (OpCode 2) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 0 ], instructionPointer: 0, halted: false } $ decode (OpCode (-1)) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 0 ], instructionPointer: 0, halted: true } $ decode (OpCode 99) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
    UnitTest.test "execute" do
      Assert.equal { memory: [ 1 ], instructionPointer: 0, halted: false } $ execute (×) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 1 ], instructionPointer: 0, halted: true } $ execute (×) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: true }
      Assert.equal { memory: [ 2 ], instructionPointer: 0, halted: false } $ execute (+) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 99 ], instructionPointer: 0, halted: false } $ execute (\_ _ → 99) (Address 0) 1 1 { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 0, 1 ], instructionPointer: 0, halted: false } $ execute (×) (Address 1) 1 1 { memory: [ 0, 0 ], instructionPointer: 0, halted: false }
    UnitTest.test "write" do
      Assert.equal { memory: [ 1 ], instructionPointer: 0, halted: false } $ write 1 (Address 0) { memory: [ 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 1 ], instructionPointer: 0, halted: true } $ write 1 (Address 0) { memory: [ 0 ], instructionPointer: 0, halted: true }
      Assert.equal { memory: [ 0, 2 ], instructionPointer: 0, halted: false } $ write 2 (Address 1) { memory: [ 0, 1 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 0, 2 ], instructionPointer: 0, halted: true } $ write 2 (Address 1) { memory: [ 0, 1 ], instructionPointer: 0, halted: true }
    UnitTest.test "step" do
      Assert.equal { memory: [], instructionPointer: 4, halted: false } $ step { memory: [], instructionPointer: 0, halted: false }
      Assert.equal { memory: [], instructionPointer: 4, halted: true } $ step { memory: [], instructionPointer: 0, halted: true }
    UnitTest.test "getCurrentInstructionAddress" do
      Assert.equal (Address 0) $ getCurrentInstructionAddress { memory: [], instructionPointer: 0, halted: false }
      Assert.equal (Address 0) $ getCurrentInstructionAddress { memory: [], instructionPointer: 0, halted: true }
      Assert.equal (Address 1) $ getCurrentInstructionAddress { memory: [], instructionPointer: 1, halted: false }
      Assert.equal (Address 1) $ getCurrentInstructionAddress { memory: [], instructionPointer: 1, halted: true }
    UnitTest.test "offset" do
      Assert.equal (Address 0) $ offset 0 (Address 0)
      Assert.equal (Address 1) $ offset 1 (Address 0)
      Assert.equal (Address 0) $ offset (-1) (Address 1)
    UnitTest.test "tick" do
      Assert.equal { memory: [ 2, 0, 0, 0 ], instructionPointer: 0, halted: false } $ tick { memory: [ 1, 0, 0, 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 2, 0, 0, 0 ], instructionPointer: 0, halted: true } $ tick { memory: [ 1, 0, 0, 0 ], instructionPointer: 0, halted: true }
      Assert.equal { memory: [ 4, 0, 0, 0 ], instructionPointer: 0, halted: false } $ tick { memory: [ 2, 0, 0, 0 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 99 ], instructionPointer: 0, halted: true } $ tick { memory: [ 99 ], instructionPointer: 0, halted: false }
      Assert.equal { memory: [ 2, 1, 0, 0, 0 ], instructionPointer: 1, halted: false } $ tick { memory: [ 1, 1, 0, 0, 0 ], instructionPointer: 1, halted: false }
    UnitTest.test "run" do
      Assert.equal [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ] $ run [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
      Assert.equal [ 2, 0, 0, 0, 99 ] $ run [ 1, 0, 0, 0, 99 ]
      Assert.equal [ 2, 3, 0, 6, 99 ] $ run [ 2, 3, 0, 3, 99 ]
      Assert.equal [ 2, 4, 4, 5, 99, 9801 ] $ run [ 2, 4, 4, 5, 99, 0 ]
      Assert.equal [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ] $ run [ 1, 1, 1, 4, 99, 5, 6, 0, 99 ]
    UnitTest.test "init" do
      Assert.equal [ 0, 1, 2 ] $ init 1 2 [ 0, 0, 0 ]
      Assert.equal [ 0, 1, 2, 0 ] $ init 1 2 [ 0, 0, 0, 0 ]
    UnitTest.test "getOutput" do
      Assert.equal 1 $ getOutput [ 1, 0, 0, 0, 0, 0 ]
    UnitTest.test "findInputs" do
      Assert.equal (Just (Tuple 4 4)) $ findInputs [ 1, 0, 0, 0, 99 ] (Tuple 0 5) (Tuple 0 5) 198
