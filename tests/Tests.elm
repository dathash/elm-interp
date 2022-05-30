module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (..)


interpTests : Test
interpTests =
    describe "Interp"
        [ test "interp NumC" <|
            \_ -> 
                NumC 4
                    |> interp
                    |> Expect.equal (NumV 4)
        , test "interp StringC" <|
            \_ -> 
                StringC "Hey!"
                    |> interp
                    |> Expect.equal (StringV "Hey!")
        , test "interp BoolC" <|
            \_ -> 
                BoolC True
                    |> interp
                    |> Expect.equal (BoolV True)
        ]

serializeTests : Test
serializeTests =
    describe "Serialize"
        [ test "serialize NumV" <|
            \_ -> 
                NumV 4
                    |> serialize
                    |> Expect.equal "4"
        , test "serialize StringV" <|
            \_ -> 
                StringV "Hey!"
                    |> serialize
                    |> Expect.equal "Hey!"
        , test "serialize BoolV" <|
            \_ -> 
                BoolV True
                    |> serialize
                    |> Expect.equal "True"
        ]

