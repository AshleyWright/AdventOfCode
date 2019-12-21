{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "advent-of-code-2019"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "node-fs-aff"
    , "node-process"
    , "prelude"
    , "psci-support"
    , "simple-json"
    , "sized-vectors"
    , "test-unit"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
