module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (run, runTestWith)
import Test.Unit.Output.Fancy (runTest)
import Test.Day01 as Day01
import Test.Day02 as Day02

main ∷ Effect Unit
main =
  run
    $ runTestWith runTest do
        Day01.test
        Day02.test
