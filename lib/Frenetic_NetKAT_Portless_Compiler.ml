open Core.Std

(* TODO *)
let to_portfull topo p = p 

(* let abs_loc_to_host id = assert (is_loc_host id); (Int64.shift_right_logical id 1)
let abs_loc_to_switch id = assert (is_loc_switch id); (Int64.shift_right_logical id 1)

let src_to_dst_list ~src_loc topo =
  List.fold topo ~init:[]
    ~f:(fun acc (src, dst) ->
        match dst with
        | Switch (dst_id, dst_pt) ->
          if link_to_abs_loc src = src_loc then
            (dst_id, dst_pt) :: acc
          else
            acc
        | _ -> acc)

let dst_to_src_list ~dst_loc topo =
  List.fold topo ~init:[]
    ~f:(fun acc (src, dst) ->
        match src with
        | Switch (src_id, src_pt) ->
          if link_to_abs_loc dst = dst_loc then
            (src_id, src_pt) :: acc
          else
            acc
        | _ -> acc)

module Portful = Frenetic_NetKAT

let rec portify_pred pred topo =
  match pred with
  | True -> Portful.True
  | False -> Portful.False
  | And (pred1, pred2) -> Portful.And (portify_pred pred1 topo, portify_pred pred2 topo)
  | Or (pred1, pred2) -> Portful.Or (portify_pred pred1 topo, portify_pred pred2 topo)
  | Neg (pred) -> Portful.Neg (portify_pred pred topo)
  | Test (header, value, mask) -> match header with
    | EthSrc -> Portful.Test (Portful.EthSrc value)
    | EthDst -> Portful.Test (Portful.EthDst value)
    | Vlan -> Portful.Test (Portful.Vlan (Int64.to_int_exn value))
    | VlanPcp -> Portful.Test (Portful.VlanPcp (Int64.to_int_exn value))
    | EthType -> Portful.Test (Portful.EthType (Int64.to_int_exn value))
    | IPProto -> Portful.Test (Portful.IPProto (Int64.to_int_exn value))
    | IP4Src -> Portful.Test (Portful.IP4Src (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | IP4Dst -> Portful.Test (Portful.IP4Dst (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | TCPSrcPort -> Portful.Test (Portful.TCPSrcPort (Int64.to_int_exn value))
    | TCPDstPort -> Portful.Test (Portful.TCPDstPort (Int64.to_int_exn value))
    | AbstractLoc -> Portful.Test (Portful.Switch (abs_loc_to_switch value))
    | From ->
      let from_list = src_to_dst_list ~src_loc:value topo in
      List.fold from_list
        ~init:(Portful.False)
        ~f:(fun acc (sw, pt) ->
            Portful.Or (acc, Portful.And (
                (Portful.Test (Portful.Switch sw)),
                (Portful.Test (Portful.Location (Portful.Physical (Int32.of_int64_exn pt)))))))

let rec portify (portless_pol: policy) topo: Portful.policy =
  match portless_pol with
  | Union (pol1, pol2) -> Portful.Union (portify pol1 topo, portify pol2 topo)
  | Seq (pol1, pol2) -> Portful.Seq (portify pol1 topo, portify pol2 topo)
  | Star pol -> Portful.Star (portify pol topo)
  | Filter pred -> Portful.Filter (portify_pred pred topo)
  | Mod (header, value, mask) -> match header with
    | EthSrc -> Portful.Mod (Portful.EthSrc value)
    | EthDst -> Portful.Mod (Portful.EthDst value)
    | Vlan -> Portful.Mod (Portful.Vlan (Int64.to_int_exn value))
    | VlanPcp -> Portful.Mod (Portful.VlanPcp (Int64.to_int_exn value))
    | EthType -> Portful.Mod (Portful.EthType (Int64.to_int_exn value))
    | IPProto -> Portful.Mod (Portful.IPProto (Int64.to_int_exn value))
    | IP4Src ->  Portful.Mod (Portful.IP4Src (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | IP4Dst ->  Portful.Mod (Portful.IP4Dst (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | TCPSrcPort -> Portful.Mod (Portful.TCPSrcPort (Int64.to_int_exn value))
    | TCPDstPort -> Portful.Mod (Portful.TCPDstPort (Int64.to_int_exn value))
    | AbstractLoc ->
      let sw_port_list = dst_to_src_list ~dst_loc:value topo in
      List.fold sw_port_list
        ~init:(if is_loc_host value then Portful.drop else Portful.Filter (Portful.Test (Portful.Switch (abs_loc_to_switch value))))
        ~f:(fun acc (sw, mod_pt) ->
            let portful_test = Portful.Test (Portful.Switch sw) in
            let portful_mod = Portful.Mod (Portful.Location (Portful.Physical (Int32.of_int64_exn mod_pt))) in
            Portful.Union (acc, Portful.Seq (Portful.Filter portful_test, portful_mod)))
    | From -> Portful.id *)
