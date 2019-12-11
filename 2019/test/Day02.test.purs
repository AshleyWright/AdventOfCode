module Test.Day02 where

import Prelude
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Test.Unit as UnitTest
import Test.Unit.Assert as Assert
import IntcodeComputer (Address(..), Computer(..), OpCode(..), decode, execute, fetch, getCurrentInstructionAddress, hasHalted, indirect, offset, run, step, tick, write)
import GravityAssist (findInputs, getOutput, init)
import Common ((×))

test ∷ UnitTest.TestSuite
test =
  UnitTest.suite "02 1202 Program Alarm" do
    UnitTest.test "fetch" do
      Assert.equal 1 $ fetch (Address 0) (Computer [ 1 ] 0 false)
      Assert.equal 1 $ fetch (Address 0) (Computer [ 1 ] 0 true)
      Assert.equal 0 $ fetch (Address 0) (Computer [] 0 false)
      Assert.equal 0 $ fetch (Address 0) (Computer [] 0 true)
    UnitTest.test "indirect" do
      Assert.equal 2 $ indirect (Address 0) (Computer [ 1, 2 ] 0 false)
      Assert.equal 2 $ indirect (Address 0) (Computer [ 1, 2 ] 0 true)
      Assert.equal 1 $ indirect (Address 1) (Computer [ 0, 1 ] 0 false)
      Assert.equal 1 $ indirect (Address 1) (Computer [ 0, 1 ] 0 true)
      Assert.equal 0 $ indirect (Address 0) (Computer [ 2, 1 ] 0 false)
      Assert.equal 0 $ indirect (Address 0) (Computer [ 2, 1 ] 0 true)
    UnitTest.test "decode" do
      Assert.equal (Computer [ 2 ] 0 false) $ decode (OpCode 1) (Address 0) 1 1 (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 2 ] 0 true) $ decode (OpCode 1) (Address 0) 1 1 (Computer [ 0 ] 0 true)
      Assert.equal (Computer [ 0, 2 ] 0 true) $ decode (OpCode 1) (Address 1) 1 1 (Computer [ 0, 1 ] 0 true)
      Assert.equal (Computer [ 1 ] 0 false) $ decode (OpCode 2) (Address 0) 1 1 (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 0 ] 0 false) $ decode (OpCode (-1)) (Address 0) 1 1 (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 0 ] 0 true) $ decode (OpCode 99) (Address 0) 1 1 (Computer [ 0 ] 0 false)
    UnitTest.test "execute" do
      Assert.equal (Computer [ 1 ] 0 false) $ execute (×) (Address 0) 1 1 (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 1 ] 0 true) $ execute (×) (Address 0) 1 1 (Computer [ 0 ] 0 true)
      Assert.equal (Computer [ 2 ] 0 false) $ execute (+) (Address 0) 1 1 (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 99 ] 0 false) $ execute (\_ _ → 99) (Address 0) 1 1 (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 0, 1 ] 0 false) $ execute (×) (Address 1) 1 1 (Computer [ 0, 0 ] 0 false)
    UnitTest.test "write" do
      Assert.equal (Computer [ 1 ] 0 false) $ write 1 (Address 0) (Computer [ 0 ] 0 false)
      Assert.equal (Computer [ 1 ] 0 true) $ write 1 (Address 0) (Computer [ 0 ] 0 true)
      Assert.equal (Computer [ 0, 2 ] 0 false) $ write 2 (Address 1) (Computer [ 0, 1 ] 0 false)
      Assert.equal (Computer [ 0, 2 ] 0 true) $ write 2 (Address 1) (Computer [ 0, 1 ] 0 true)
    UnitTest.test "step" do
      Assert.equal (Computer [] 4 false) $ step (Computer [] 0 false)
      Assert.equal (Computer [] 4 true) $ step (Computer [] 0 true)
    UnitTest.test "hasHalted" do
      Assert.equal true $ hasHalted (Computer [] 0 true)
      Assert.equal false $ hasHalted (Computer [] 0 false)
    UnitTest.test "getCurrentInstructionAddress" do
      Assert.equal (Address 0) $ getCurrentInstructionAddress (Computer [] 0 false)
      Assert.equal (Address 0) $ getCurrentInstructionAddress (Computer [] 0 true)
      Assert.equal (Address 1) $ getCurrentInstructionAddress (Computer [] 1 false)
      Assert.equal (Address 1) $ getCurrentInstructionAddress (Computer [] 1 true)
    UnitTest.test "offset" do
      Assert.equal (Address 0) $ offset 0 (Address 0)
      Assert.equal (Address 1) $ offset 1 (Address 0)
      Assert.equal (Address 0) $ offset (-1) (Address 1)
    UnitTest.test "tick" do
      Assert.equal (Computer [ 2, 0, 0, 0 ] 0 false) $ tick (Computer [ 1, 0, 0, 0 ] 0 false)
      Assert.equal (Computer [ 2, 0, 0, 0 ] 0 true) $ tick (Computer [ 1, 0, 0, 0 ] 0 true)
      Assert.equal (Computer [ 4, 0, 0, 0 ] 0 false) $ tick (Computer [ 2, 0, 0, 0 ] 0 false)
      Assert.equal (Computer [ 99 ] 0 true) $ tick (Computer [ 99 ] 0 false)
      Assert.equal (Computer [ 2, 1, 0, 0, 0 ] 1 false) $ tick (Computer [ 1, 1, 0, 0, 0 ] 1 false)
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
