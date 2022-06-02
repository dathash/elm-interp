module Main exposing (..)

import Html exposing (text)

base_env : Env
base_env = (Env [(Binding "+" (OpV "+")),
                 (Binding "-" (OpV "-")),
                 (Binding "*" (OpV "*")),
                 (Binding "/" (OpV "/")),
                 (Binding "<=" (OpV "<=")),
                 (Binding "equal?" (OpV "equal?")),
                 (Binding "True" (BoolV True)),
                 (Binding "False" (BoolV False)),
                 (Binding "error" (OpV "error"))])

type ExprC
    = NumC Float
    | BoolC Bool
    | StringC String
    | IfC ExprC ExprC ExprC
    | AppC (List ExprC) ExprC

-- StringC "Hi!"
--     |> interp
--     |> serialize
--     |> Html.text
-- Interprets an Expression in JILI5.
--    | LamC (List String) ExprC
-- | MaybeIfC ExprC ExprC ExprC
-- type MaybeIfC
--   = IfC { cond : ExprC, ifT : ExprC, ifF : ExprC }
--   | NotABool
-- LamC
-- AppC
-- IfC

type alias Env = { bindings : (List Binding)}
type alias Binding = {name : String, value : Value}

type Value
    = NumV Float
    | BoolV Bool
    | StringV String
    | CloV (List String) ExprC Env
    | OpV String

main =
    base_env
        |> interp (IfC (BoolC True) (NumC 1) (NumC 0)) 
        |> serialize
        |> Html.text

--================== ENV FUNCTIONS ====================
envLookup : String -> Env -> Value
envLookup name env =
    case env.bindings of
        [] -> StringV "Binding not found"
        f :: r -> 
            if name == f.name then
                f.value
            else
                envLookup name (Env r)

envExtend : String -> Value -> Env -> Env
envExtend name value env =
    case env.bindings of
        [] -> (Env [(Binding name value)])
        f :: r -> 
            if name == f.name then
                (Env ((Binding name value) :: r))
            else
                (Env (f :: (envExtend name value (Env r)).bindings))

-- foldExtend : (List String) -> (List Value) -> Env -> (List Binding)
-- foldExtend names values env =
--     case names of
--         [] -> []
--         fNames :: rNames ->
--             case values of
--                 [] -> []
--                 fValues :: rValues ->
--                         (envExtend fNames fValues env).bindings 
--                         :: (foldExtend rNames rValues env)
--=====================================================

--============= INTERP HELPER FUNCTIONS ===============
interp_all_args : (List ExprC) -> Env -> (List Value)
interp_all_args args env =
    case args of
        [] -> []
        f :: r -> 
            (interp f env) :: (interp_all_args r env)

-- evalOp : String -> (List Value) -> Value
-- evalOp op args = 
--     case (op, args) of
--         ("+", [a, b]) -> (NumV (a + b))
--=====================================================

--================ INTERP FUNCTIONS ===================
-- Maybe there's a way to implement this with map

interp : ExprC -> Env -> Value
interp expr env =
    case expr of
        NumC n ->
            NumV n

        BoolC b ->
            BoolV b

        StringC s ->
            StringV s

        IfC condition ifTrue ifFalse ->
            case interp condition env of
                BoolV b ->
                    if b then
                        interp ifTrue env
                    else
                        interp ifFalse env
                _ ->
                    StringV "Error Placeholder (Cond not a bool)"

        AppC p b ->
            case interp b env of
                CloV param body cloEnv ->
                    let 
                        args : (List Value)
                        args = 
                            interp_all_args p env
                    in
                        if List.length args == List.length param then
                            -- interp body (List.foldl envExtend param args cloEnv)
                            (StringV "Working on this")
                        else
                            (StringV "Invalid number of arguments passed")
                OpV op ->
                    (StringV "Arithmetic Logic Here")
                _ -> (StringV "Invalid Function Application")

--=====================================================


-- LamC params body ->
--     CloV params body env
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
        
        CloV p b e ->
            "#<procedure>"

        OpV op ->
            "#<primop>"


stringFromBool : Bool -> String
stringFromBool value =
    if value then
        "True"

    else
        "False"
