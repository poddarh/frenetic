open Core.Std
open Frenetic_OpenFlow

let _ =
  switchId_to_yojson 1L
  |> Yojson.Safe.to_string
  |> printf "%s"
