module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


interpTests : Test
interpTests =
    describe "Interp"
        [ test "interp NumC" <|
            \_ ->
                base_env
                    |> interp (NumC 4)
                    |> Expect.equal (NumV 4)
        , test "interp StringC" <|
            \_ ->
                base_env
                    |> interp (StringC "Hey!")
                    |> Expect.equal (StringV "Hey!")
        , test "interp BoolC" <|
            \_ ->
                base_env
                    |> interp (BoolC True)
                    |> Expect.equal (BoolV True)
        , test "interp IfC" <|
            \_ ->
                base_env
                    |> interp (IfC (BoolC True) (NumC 1) (NumC 2))
                    |> Expect.equal (NumV 1)
        ]

envLookupTests : Test
envLookupTests = 
    describe  "Env Lookup"
        [ test "Lookup empty" <|
            \_ -> 
                (Env [])
                    |> envLookup "+"
                    |> Expect.equal (StringV "Binding not found")
        , test "Lookup a value" <|
            \_ -> 
                base_env
                    |> envLookup "+"
                    |> Expect.equal (OpV "+")
        , test "Lookup another value" <|
            \_ -> 
                base_env
                    |> envLookup "False"
                    |> Expect.equal (BoolV False)]

envExtendTests : Test
envExtendTests = 
    describe "Env Extend"
        [test "Empty Env" <|
            \_ ->
                (Env [])
                    |> envExtend "+" (OpV "+")
                    |> Expect.equal (Env [(Binding "+" (OpV "+"))])
        , test "Adding a new binding on existing env" <|
            \_ -> 
                (Env [(Binding "hi" (StringV "hi"))])
                    |> envExtend "+" (OpV "+")
                    |> Expect.equal (Env [
                        (Binding "hi" (StringV "hi")),
                        (Binding "+" (OpV "+"))])
        , test "Modifying a binding" <|
            \_ -> 
                (Env [(Binding "hi" (StringV "hi")),
                      (Binding "+" (OpV "+"))])
                    |> envExtend "+" (StringV "plus")
                    |> Expect.equal (Env [
                        (Binding "hi" (StringV "hi")),
                        (Binding "+" (StringV "plus"))
                    ])]

interpAllArgsTests : Test
interpAllArgsTests = 
    describe "Interp All Args"
        [test "interp all" <|
            \_ ->
                base_env
                    |> interp_all_args [(NumC 4), (StringC "hi"), (BoolC True)]
                    |> Expect.equalLists [(NumV 4), (StringV "hi"), (BoolV True)]]


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
        , test "serialize IfC" <|
            \_ ->
                base_env
                    |> interp (IfC (BoolC True) (NumC 1) (NumC 2))
                    |> serialize
                    |> Expect.equal "1"
        , test "serialize CloV" <|
            \_ ->
                CloV ["Hello", "Yall"] (NumC 1) (Env [(Binding "+" (NumV 3))])
                |> serialize
                |> Expect.equal "#<procedure>"
        , test "serialize OpV" <|
            \_ ->
                OpV "+"
                |> serialize
                |> Expect.equal "#<primop>"
        ]
