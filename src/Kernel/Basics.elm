module Kernel.Basics exposing (idiv, modBy, remainderBy)

import EvalResult
import Types exposing (Eval, Value)
import Value


modBy : Int -> Int -> Eval Int
modBy modulus x _ env =
    if modulus == 0 then
        EvalResult.fail <| Value.todo env "modBy 0 is not allowed"

    else
        EvalResult.succeed (Basics.modBy modulus x)


remainderBy : Int -> Int -> Eval Int
remainderBy divisor x _ env =
    if divisor == 0 then
        EvalResult.fail <| Value.todo env "remainderBy 0 is not allowed"

    else
        EvalResult.succeed (Basics.remainderBy divisor x)


idiv : Int -> Int -> Eval Int
idiv a b _ env =
    if b == 0 then
        EvalResult.fail <| Value.todo env "integer division by zero"

    else
        EvalResult.succeed (a // b)
