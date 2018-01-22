open Core.Std
open Frenetic_NetKAT_Portless

val single: int -> topo
val minimal: topo
val linear: int -> topo
val tree: int -> int -> topo

type topo_name =
  | Tree of int * int
  | Linear of int
  | Single of int
  | Minimal

val topo_from_name: topo_name -> topo