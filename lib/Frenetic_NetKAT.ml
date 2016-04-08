open Sexplib.Conv
open Core.Std

(** NetKAT Syntax *)

(** {1 Basics} *)
open Frenetic_Packet

(* thrown whenever local policy is expected, but global policy
  (i.e. policy containing links) is encountered *)
exception Non_local

type switchId = Frenetic_OpenFlow.switchId [@@deriving sexp, compare, eq, yojson]
type portId = Frenetic_OpenFlow.portId [@@deriving sexp, compare, eq, yojson]
type payload = Frenetic_OpenFlow.payload [@@deriving sexp]
type vswitchId = int64 [@@deriving sexp, compare, eq, yojson]
type vportId = int64 [@@deriving sexp, compare, eq, yojson]
type vfabricId = int64 [@@deriving sexp, compare, eq, yojson]

(** {2 Headers} *)

type location =
  | Physical of int32
  | FastFail of int32 list
  | Pipe of string
  | Query of string
  [@@deriving sexp, compare, yojson]

type ip = nwAddr * int32 [@@deriving sexp]

let ip_to_yojson (addr, mask : ip) : Yojson.Safe.json =
  let open Yojson.Safe.Util in
  let addr = ("addr", `String (Frenetic_Packet.string_of_ip addr)) in
  let mask = Int32.to_int_exn mask |> function
    | 32 -> []
    | m -> [("mask", `Int m)] in
  `Assoc (addr :: mask)

let ip_of_yojson (json : Yojson.Safe.json) : [ `Ok of ip | `Error of string ] =
  try
    let open Yojson.Safe.Util in
    let addr = json |> member "addr" |> to_string |> Frenetic_Packet.ip_of_string in
    let mask = json |> member "mask" |> function
      | `Null -> 32 |> Int32.of_int_exn
      | x -> x |> to_int |> Int32.of_int_exn in
    `Ok (addr, mask)
  with e -> `Error (Exn.to_string e)

type header_val =
  | Switch of switchId
  | Location of location
  | EthSrc of dlAddr
  | EthDst of dlAddr
  | Vlan of int16
  | VlanPcp of dlVlanPcp
  | EthType of dlTyp
  | IPProto of nwProto
  | IP4Src of ip
  | IP4Dst of ip
  | TCPSrcPort of tpPort
  | TCPDstPort of tpPort
  | VSwitch of vswitchId
  | VPort of vportId
  | VFabric of vfabricId
  [@@deriving sexp, yojson]


(** {3 Policies} *)

type pred =
  | True
  | False
  | Test of header_val
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
  [@@deriving sexp, yojson]

type policy =
  | Filter of pred
  | Mod of header_val
  | Union of policy * policy
  | Seq of policy * policy
  | Star of policy
  | Link of switchId * portId * switchId * portId
  | VLink of vswitchId * vportId * vswitchId * vportId
  [@@deriving sexp, yojson]

let id = Filter True
let drop = Filter False


(** {4 Applications} *)

type action = Frenetic_OpenFlow.action

type switch_port = switchId * portId [@@deriving sexp]
type host = Frenetic_Packet.dlAddr * Frenetic_Packet.nwAddr [@@deriving sexp]

type bufferId = Int32.t [@@deriving sexp] (* XXX(seliopou): different than Frenetic_OpenFlow *)

type event =
  | PacketIn of string * switchId * portId * payload * int
  | Query of string * int64 * int64
  | SwitchUp of switchId * portId list
  | SwitchDown of switchId
  | PortUp of switch_port
  | PortDown of switch_port
  | LinkUp of switch_port * switch_port
  | LinkDown of switch_port * switch_port
  | HostUp of switch_port * host
  | HostDown of switch_port * host
  [@@deriving sexp]

(** {5 Applications} **)
let string_of_fastfail = Frenetic_OpenFlow.format_list ~to_string:Int32.to_string
