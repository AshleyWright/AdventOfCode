module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Day01 as Day01
import Test.Day02 as Day02
import Test.Day03 as Day03
import Test.Day04 as Day04

main ∷ Effect Unit
main =
  runTest do
    Day01.test
    Day02.test
    Day03.test
    Day04.test
