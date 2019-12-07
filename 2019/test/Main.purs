module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Day01TheTyrannyOfTheRocketEquation as Day01

main :: Effect Unit
main = do
  runTest Day01.test
