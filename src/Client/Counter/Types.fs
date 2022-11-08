module Counter.Types

open Shared
open Characters

type Model = Result<Character,string>

type Msg =
    | Str of string
    // | Increment
    // | Decrement
    // | Reset
