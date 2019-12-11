module Test.Day01 where

import Prelude
import Test.Unit as UnitTest
import Test.Unit.Assert as Assert
import CalculateFuelRequirement (getRequiredFuel)
import CalculateFuelRequirementRecursive (getRequiredFuelRecursive)

test ∷ UnitTest.TestSuite
test =
  UnitTest.suite "01 The Tyranny of the Rocket Equation" do
    UnitTest.test "getRequiredFuel" do
      Assert.equal 2.0 $ getRequiredFuel 12.0
      Assert.equal 2.0 $ getRequiredFuel 14.0
      Assert.equal 654.0 $ getRequiredFuel 1969.0
      Assert.equal 33583.0 $ getRequiredFuel 100756.0
    UnitTest.test "getRequiredFuelRecursive" do
      Assert.equal 2.0 $ getRequiredFuelRecursive 14.0
      Assert.equal 966.0 $ getRequiredFuelRecursive 1969.0
      Assert.equal 50346.0 $ getRequiredFuelRecursive 100756.0
