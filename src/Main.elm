module Main exposing (..)

import Html exposing (text)


type ExprC
    = NumC Float
    | BoolC Bool
    | StringC String
    | IfC ExprC ExprC ExprC



-- | MaybeIfC ExprC ExprC ExprC
-- type MaybeIfC
--   = IfC { cond : ExprC, ifT : ExprC, ifF : ExprC }
--   | NotABool
-- LamC
-- AppC
-- IfC


type Value
    = NumV Float
    | BoolV Bool
    | StringV String



-- CloV


main =
    IfC (BoolC True) (NumC 1) (NumC 0)
        |> interp
        |> serialize
        |> Html.text



-- StringC "Hi!"
--     |> interp
--     |> serialize
--     |> Html.text
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

        IfC condition ifTrue ifFalse ->
            case interp condition of
                BoolV b ->
                    if b then
                        interp ifTrue

                    else
                        interp ifFalse

                _ ->
                    StringV "Error Placeholder (Cond not a bool)"



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
