module Test.Generated.Main exposing (main)

import Tests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 10600922488107
        , processes = 10
        , globs =
            []
        , paths =
            [ "/Users/salito/Documents/GitHub/elm-interp/tests/Tests.elm"
            ]
        }
        [ ( "Tests"
          , [ Test.Runner.Node.check Tests.interpTests
            , Test.Runner.Node.check Tests.envLookupTests
            , Test.Runner.Node.check Tests.envExtendTests
            , Test.Runner.Node.check Tests.interpAllArgsTests
            , Test.Runner.Node.check Tests.serializeTests
            ]
          )
        ]