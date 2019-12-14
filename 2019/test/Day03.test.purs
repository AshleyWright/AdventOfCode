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
      Assert.equal { from: { x: 1, y: 1, signalDelay: 0 }, to: { x: 1, y: 3, signalDelay: 2 } } $ makeLine { x: 1, y: 1, signalDelay: 0 } (Tuple "U" 2)
      Assert.equal { from: { x: 1, y: 1, signalDelay: 0 }, to: { x: 3, y: 1, signalDelay: 2 } } $ makeLine { x: 1, y: 1, signalDelay: 0 } (Tuple "R" 2)
      Assert.equal { from: { x: 1, y: 1, signalDelay: 0 }, to: { x: 1, y: (-1), signalDelay: 2 } } $ makeLine { x: 1, y: 1, signalDelay: 0 } (Tuple "D" 2)
      Assert.equal { from: { x: 1, y: 1, signalDelay: 0 }, to: { x: (-1), y: 1, signalDelay: 2 } } $ makeLine { x: 1, y: 1, signalDelay: 0 } (Tuple "L" 2)
    UnitTest.test "manhattenDistance" do
      Assert.equal 2 $ manhattenDistance { x: 1, y: 1 }
      Assert.equal 2 $ manhattenDistance { x: (-1), y: 1 }
      Assert.equal 2 $ manhattenDistance { x: 1, y: (-1) }
      Assert.equal 2 $ manhattenDistance { x: (-1), y: (-1) }
    UnitTest.test "orientation" do
      Assert.equal Vertical $ orientation { from: { x: 0, y: (-1) }, to: { x: 0, y: 1 } }
      Assert.equal Horizontal $ orientation { from: { x: (-1), y: 0 }, to: { x: 1, y: 0 } }
      Assert.equal Diagonal $ orientation { from: { x: (-1), y: 0 }, to: { x: 1, y: 1 } }
    UnitTest.test "gradient" do
      Assert.equal 0.0 $ gradient { from: { x: 0, y: 0 }, to: { x: 1, y: 0 } }
      Assert.equal 1.0 $ gradient { from: { x: 0, y: 0 }, to: { x: 1, y: 1 } }
      Assert.equal 0.5 $ gradient { from: { x: 0, y: 0 }, to: { x: 2, y: 1 } }
      Assert.equal 2.0 $ gradient { from: { x: 0, y: 0 }, to: { x: 1, y: 2 } }
      Assert.equal (-1.0) $ gradient { from: { x: 0, y: 0 }, to: { x: 1, y: (-1) } }
    UnitTest.test "intersect" do
      Assert.equal 1.0 $ intersect { from: { x: (-1), y: 1 }, to: { x: 1, y: 1 } }
    UnitTest.test "areParrallel" do
      Assert.equal true $ areParrallel { from: { x: (-1), y: 1 }, to: { x: 1, y: 1 } } { from: { x: (-1), y: 2 }, to: { x: 1, y: 2 } }
      Assert.equal true $ areParrallel { from: { x: 1, y: (-1) }, to: { x: 1, y: 1 } } { from: { x: 2, y: (-1) }, to: { x: 2, y: 1 } }
      Assert.equal true $ areParrallel { from: { x: 0, y: (-1) }, to: { x: 1, y: 0 } } { from: { x: (-1), y: 0 }, to: { x: 0, y: 1 } }
      Assert.equal false $ areParrallel { from: { x: (-1), y: 1 }, to: { x: 1, y: 1 } } { from: { x: 2, y: (-1) }, to: { x: 2, y: 1 } }
    UnitTest.test "intersection" do
      Assert.equal Nothing $ intersection { from: { x: 0, y: (-1) }, to: { x: 1, y: 0 } } { from: { x: (-1), y: 0 }, to: { x: 0, y: 1 } }
      Assert.equal (Just { x: 0, y: 0 }) $ intersection { from: { x: (-1), y: 0 }, to: { x: 1, y: 0 } } { from: { x: 0, y: (-1) }, to: { x: 0, y: 1 } }
      Assert.equal (Just { x: 0, y: 0 }) $ intersection { from: { x: 0, y: (-1) }, to: { x: 0, y: 1 } } { from: { x: (-1), y: 0 }, to: { x: 1, y: 0 } }
      Assert.equal (Just { x: 0, y: 0 }) $ intersection { from: { x: (-1), y: (-1) }, to: { x: 1, y: 1 } } { from: { x: (-1), y: 1 }, to: { x: 1, y: (-1) } }
      Assert.equal (Just { x: 1, y: 0 }) $ intersection { from: { x: 0, y: (-1) }, to: { x: 1, y: 0 } } { from: { x: (-1), y: 0 }, to: { x: 1, y: 0 } }
