module Counter.State

open Elmish
open Types
open Shared
open Pinyin
// open Characters

let init () : Model * Cmd<Msg> =
  Error("Type a pinyin above") , []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | Str str ->
        match (StringToPinyin str) with
        | Some pinyin -> 
          Error("This function is not yet implemented"), []          
          //(TryGetCharacterFromPinyin pinyin, [])
        | None -> Error("No pinyin found"), []

