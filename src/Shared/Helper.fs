module Shared.Helper

let fst li =
  match li with [] -> None | h::_ -> Some h
