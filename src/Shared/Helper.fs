module Shared.Helper

let first li =
  match li with [] -> None | h::_ -> Some h