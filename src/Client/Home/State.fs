module Home.State

open Elmish
open Types

open Shared
open Pinyin

let init () : Model * Cmd<Msg> =
  Fail("Type a pinyin above") , []

let update msg model : Model * Cmd<Msg> =
    match msg with
    | ChangeStr str ->
        match (StringToPinyin str) with
        | Some pinyin -> Pinyin(pinyin), []
        | None -> Fail("No pinyin found"), []
