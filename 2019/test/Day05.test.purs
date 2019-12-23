module Test.Day05 where

import Prelude (discard, ($))
import Data.Vec (empty)
import Test.Unit as UnitTest
import Test.Unit.Assert as Assert
import AirConDiagnostic (Operation(..), ParameterMode(..), getOperation, getParameterModes, parameterCount)
import Common ((▶))

test ∷ UnitTest.TestSuite
test =
  UnitTest.suite "05 Sunny with a Chance of Asteroids" do
    UnitTest.test "getOperation" do
      Assert.equal Add $ getOperation 1
      Assert.equal Multiply $ getOperation 2
      Assert.equal Input $ getOperation 3
      Assert.equal Output $ getOperation 4
      Assert.equal Halt $ getOperation 99
      Assert.equal Add $ getOperation 101
      Assert.equal Add $ getOperation 11101
      Assert.equal Add $ getOperation 10001
    UnitTest.test "parameterCount" do
      Assert.equal 3 $ parameterCount Add
      Assert.equal 3 $ parameterCount Multiply
      Assert.equal 1 $ parameterCount Input
      Assert.equal 1 $ parameterCount Output
      Assert.equal 0 $ parameterCount Halt
    UnitTest.test "getParameterModes" do
      Assert.equal (Immediate ▶ Position ▶ Immediate ▶ Position ▶ empty) $ getParameterModes 10100
      Assert.equal (Position ▶ Immediate ▶ Position ▶ Position ▶ empty) $ getParameterModes 1000
      Assert.equal (Position ▶ Immediate ▶ Position ▶ Position ▶ empty) $ getParameterModes 1099
      Assert.equal (Position ▶ Immediate ▶ Immediate ▶ Position ▶ empty) $ getParameterModes 1001
