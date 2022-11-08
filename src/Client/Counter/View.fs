module Counter.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Types

let simpleButton txt action dispatch =
  div
    [ ClassName "column is-narrow" ]
    [ a
        [ ClassName "button"
          OnClick (fun _ -> action |> dispatch) ]
        [ str txt ] ]

let root (model : Model)  dispatch =
  div
    [ ]
    [ p
        [ ClassName "control" ]
        [ input
            [ ClassName "input"
              Type "text"
              Placeholder "Type your name"
              DefaultValue ""
              AutoFocus true
              OnChange (fun ev -> !!ev.target?value |> Str |> dispatch ) ] ]
      br [ ]

      match model with
      | Ok result ->
        div
          []
          [
            span [ Style [ Color "#7700AA" ] ] [
                str (string(result.Character))
            ]     
          ]
      | Error failString -> span [] [ str failString ]
      
    ]