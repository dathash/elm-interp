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
        , seed = 47356781986363
        , processes = 8
        , globs =
            []
        , paths =
            [ "/Users/alexhartford/School/430/a9/tests/Tests.elm"
            ]
        }
        [ ( "Tests"
          , [ Test.Runner.Node.check Tests.interpTests
            , Test.Runner.Node.check Tests.serializeTests
            ]
          )
        ]