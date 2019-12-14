module Test.Day04 where

import Prelude (discard, ($))
import Test.Unit as UnitTest
import Test.Unit.Assert as Assert
import PasswordCount

test ∷ UnitTest.TestSuite
test =
  UnitTest.suite "04 Secure Container" do
    UnitTest.test "ascendingDigits" do
      Assert.equal true $ ascendingDigits 0
      Assert.equal true $ ascendingDigits 12
      Assert.equal true $ ascendingDigits 129
      Assert.equal false $ ascendingDigits 21
      Assert.equal false $ ascendingDigits 121
    UnitTest.test "equalAdjacents" do
      Assert.equal true $ equalAdjacents 11
      Assert.equal true $ equalAdjacents 112
      Assert.equal true $ equalAdjacents 211
      Assert.equal true $ equalAdjacents 2112
      Assert.equal false $ equalAdjacents 0
      Assert.equal false $ equalAdjacents 121
