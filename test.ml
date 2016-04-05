open Core.Std
open Frenetic_OpenFlow
open Frenetic_Packet

let t1 =
  switchId_to_yojson 1L
  |> Yojson.Safe.to_string
  |> printf "%s"

type ip = {
  addr : nwAddr;
  mask : (int32 [@default 32l]);
}
[@@deriving yojson]

let t2 =
  { addr = 34212l; mask=32l }
  |> ip_to_yojson
  |> Yojson.Safe.to_string
  |> printf "%s"

