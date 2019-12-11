module Test.Day03 where

import Prelude (discard, negate, ($))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Unit as UnitTest
import Test.Unit.Assert as Assert
import ClosestIntersection (Orientation(..), areParrallel, gradient, intersect, intersection, makeLine, manhattenDistance, orientation, toVector)

test ∷ UnitTest.TestSuite
test =
  UnitTest.suite "03 Crossed Wires" do
    UnitTest.test "toVector" do
      Assert.equal (Tuple "U" 1) $ toVector "U1"
      Assert.equal (Tuple "R" 1) $ toVector "R1"
      Assert.equal (Tuple "D" 1) $ toVector "D1"
      Assert.equal (Tuple "L" 1) $ toVector "L1"
      Assert.equal (Tuple "DIAG" 1) $ toVector "DIAG1"
    UnitTest.test "makeLine" do
      Assert.equal (Tuple (Tuple 1 1) (Tuple 1 3)) $ makeLine (Tuple 1 1) (Tuple "U" 2)
      Assert.equal (Tuple (Tuple 1 1) (Tuple 3 1)) $ makeLine (Tuple 1 1) (Tuple "R" 2)
      Assert.equal (Tuple (Tuple 1 (-1)) (Tuple 1 1)) $ makeLine (Tuple 1 1) (Tuple "D" 2)
      Assert.equal (Tuple (Tuple (-1) 1) (Tuple 1 1)) $ makeLine (Tuple 1 1) (Tuple "L" 2)
    UnitTest.test "manhattenDistance" do
      Assert.equal 2 $ manhattenDistance (Tuple 1 1)
      Assert.equal 2 $ manhattenDistance (Tuple (-1) 1)
      Assert.equal 2 $ manhattenDistance (Tuple 1 (-1))
      Assert.equal 2 $ manhattenDistance (Tuple (-1) (-1))
    UnitTest.test "orientation" do
      Assert.equal Vertical $ orientation (Tuple (Tuple 0 (-1)) (Tuple 0 1))
      Assert.equal Horizontal $ orientation (Tuple (Tuple (-1) 0) (Tuple 1 0))
      Assert.equal Diagonal $ orientation (Tuple (Tuple (-1) 0) (Tuple 1 1))
    UnitTest.test "gradient" do
      Assert.equal 0.0 $ gradient (Tuple (Tuple 0 0) (Tuple 1 0))
      Assert.equal 1.0 $ gradient (Tuple (Tuple 0 0) (Tuple 1 1))
      Assert.equal 0.5 $ gradient (Tuple (Tuple 0 0) (Tuple 2 1))
      Assert.equal 2.0 $ gradient (Tuple (Tuple 0 0) (Tuple 1 2))
      Assert.equal (-1.0) $ gradient (Tuple (Tuple 0 0) (Tuple 1 (-1)))
    UnitTest.test "intersect" do
      Assert.equal 1.0 $ intersect (Tuple (Tuple (-1) 1) (Tuple 1 1))
    UnitTest.test "areParrallel" do
      Assert.equal true $ areParrallel (Tuple (Tuple (-1) 1) (Tuple 1 1)) (Tuple (Tuple (-1) 2) (Tuple 1 2))
      Assert.equal true $ areParrallel (Tuple (Tuple 1 (-1)) (Tuple 1 1)) (Tuple (Tuple 2 (-1)) (Tuple 2 1))
      Assert.equal true $ areParrallel (Tuple (Tuple 0 (-1)) (Tuple 1 0)) (Tuple (Tuple (-1) 0) (Tuple 0 1))
      Assert.equal false $ areParrallel (Tuple (Tuple (-1) 1) (Tuple 1 1)) (Tuple (Tuple 2 (-1)) (Tuple 2 1))
    UnitTest.test "intersection" do
      Assert.equal Nothing $ intersection (Tuple (Tuple 0 (-1)) (Tuple 1 0)) (Tuple (Tuple (-1) 0) (Tuple 0 1))
      Assert.equal (Just (Tuple 0 0)) $ intersection (Tuple (Tuple (-1) 0) (Tuple 1 0)) (Tuple (Tuple 0 (-1)) (Tuple 0 1))
      Assert.equal (Just (Tuple 0 0)) $ intersection (Tuple (Tuple 0 (-1)) (Tuple 0 1)) (Tuple (Tuple (-1) 0) (Tuple 1 0))
      Assert.equal (Just (Tuple 0 0)) $ intersection (Tuple (Tuple (-1) (-1)) (Tuple 1 1)) (Tuple (Tuple (-1) 1) (Tuple 1 (-1)))
      Assert.equal (Just (Tuple 1 0)) $ intersection (Tuple (Tuple 0 (-1)) (Tuple 1 0)) (Tuple (Tuple (-1) 0) (Tuple 1 0))
