module Main exposing (..)

import Html exposing (text)

type ExprC
    = NumC Float
    | BoolC Bool
    | StringC String

type Value
    = NumV Float
    | BoolV Bool
    | StringV String

main =
    StringC "Hi!"
        |> interp
        |> serialize
        |> Html.text

-- Interprets an Expression in JILI5.
interp : ExprC -> Value
interp expr =
    case expr of
        NumC n ->
            NumV n
        BoolC b ->
            BoolV b
        StringC s ->
            StringV s

-- Converts a JILI5 Value to its String representation.
serialize : Value -> String
serialize val =
    case val of
        NumV f -> 
            String.fromFloat f
        BoolV b ->
            stringFromBool b
        StringV s ->
            s

stringFromBool : Bool -> String
stringFromBool value =
    if value then
        "True"
    else
        "False"

