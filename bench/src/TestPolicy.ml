open Core.Std
open Frenetic_NetKAT
open Frenetic_NetKAT_Semantics

let load_file f =
  let ic = open_in f in
  let n = Int64.to_int_exn (In_channel.length ic) in
  let s = String.create n in
  really_input ic s 0 n;
  In_channel.close ic;
  (s)

let reset_vtags pkt =
  let open HeadersValues in
  {pkt with headers = {pkt.headers with vlan=0; vlanPcp=0; vswitch=0L; vport=0L }}

let pkt_to_string packet =
    "switch=" ^ Int64.to_string packet.switch ^ "; " ^ HeadersValues.to_string packet.headers

let compare_eval_output p q pkt =
  let p_res = PacketSet.map ~f:reset_vtags (eval pkt p) in
  let q_res = PacketSet.map ~f:reset_vtags (eval pkt q) in
  PacketSet.compare p_res q_res = 0

let rec pred_to_sw_pt pred = match pred with
  | Or (pred1, pred2) -> pred_to_sw_pt pred1 @ pred_to_sw_pt pred2
  | And (Test(Switch sw), Test(Location(Physical pt))) -> [(sw, pt)]
  | _ -> failwith "Invalid Predicate"

let make_packet isw ipt mac =
  let open HeadersValues in
  let headers = {
    location = Physical ipt ;
    ethSrc = 0L ;
    ethDst = mac ;
    vlan = 0 ;
    vlanPcp = 0 ;
    vswitch = 0L ;
    vport = 0L ;
    ethType = 0;
    ipProto = 0 ;
    ipSrc = 0l ;
    ipDst = 0l ;
    tcpSrcPort = 0 ;
    tcpDstPort = 0
  } in
  let payload = Frenetic_OpenFlow.NotBuffered (Cstruct.create 0) in
  let pkt = {switch = isw; headers = headers; payload = payload} in
  pkt

let _ =
  let pol1  = Frenetic_NetKAT_Parser.pol_of_string  (load_file Sys.argv.(1)) in
  let pol2  = Frenetic_NetKAT_Parser.pol_of_string  (load_file Sys.argv.(2)) in
  let pred1 = Frenetic_NetKAT_Parser.pred_of_string (load_file Sys.argv.(3)) in
  let pred2 = Frenetic_NetKAT_Parser.pred_of_string (load_file Sys.argv.(4)) in
  let (topo, _, _, _)  = PolicyGen.parse_topo_file ~log:false  Sys.argv.(5) in

  let inputs  = pred_to_sw_pt pred1 in
  let outputs = pred_to_sw_pt pred2 in
  let hosts = List.map (PolicyGen.hosts_of_topo topo) (fun ((h, mac), (sw, pt)) -> (sw, pt), (h, mac)) in

  let from_to = List.fold_left inputs ~init:[] ~f:(fun acc (isw, ipt) ->
      List.fold_left outputs ~init:acc ~f:(fun acc' (osw, opt) ->
          let (h, mac) = List.Assoc.find_exn hosts (osw, opt) in
          ((isw, ipt), (osw, opt, mac)) :: acc')) in

  let x = List.fold from_to ~init:true ~f:(fun acc ((isw, ipt), (osw, opt, mac)) ->
      compare_eval_output pol1 pol2 (make_packet isw ipt mac) && acc) in
  if x then
    print_endline "Success!"
  else
    print_endline "Failure!";
  x