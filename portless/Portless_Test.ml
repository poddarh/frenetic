open Core.Std
open Frenetic_NetKAT_Portless
open Frenetic_NetKAT_Portless_Compiler
open Frenetic_NetKAT_Compiler
open Frenetic_NetKAT_Portless_Parser
open Tree_Topo

(* let ed = Sexp.to_string (Topology.sexp_of_edge edge) in
   let vs = Sexp.to_string (Topology.sexp_of_vertex vertex_src) in
   let vd = Sexp.to_string (Topology.sexp_of_vertex vertex_dst) in
   let ps = Sexp.to_string (Topology.sexp_of_port pt_src) in
   let pd = Sexp.to_string (Topology.sexp_of_port pt_dst) in
   print_endline ("Edge: " ^ ed);
   print_endline ("  Vertex Src: " ^ vs);
   print_endline ("  Port   Src: " ^ ps);
   print_endline ("  Vertex Dst: " ^ vd);
   print_endline ("  Port   Dst: " ^ pd);
   print_endline ""; *)

(* module HeaderValues = Map.Make(struct
    type t = header [@@deriving sexp]
    let compare = compare
  end) *)

(* type packet =
  | HeaderValues of int64 HeaderValues.t
[@@deriving sexp] *)

let _ =
  (* let hv = List.fold
      ~init:HeaderValues.empty
      ~f:(fun acc (k, v) -> HeaderValues.add acc k v)
      [(AbstractLoc, 1L); (AbstractLoc, 2L); (From, 4L)] in

     let sexp = sexp_of_packet (HeaderValues hv) in
     print_endline (Sexp.to_string sexp); *)

  (* let topo = parse_topo_from_dotfile Sys.argv.(1) in *)
  (* let str = Int64.to_string in
     let loc_str loc = if Int64.(<) loc 0L then ("h" ^ (str (Int64.(-) (default_mask) loc))) else ("s" ^ (str loc)) in
     List.iter topo ~f:(fun (sw, pt, to_loc) -> print_endline ("s" ^ str sw ^ " " ^ str pt ^ " " ^ loc_str to_loc)) *)

  (* let big_seq = List.fold ~init:(Filter True) ~f:(fun acc pol -> Seq (acc, pol)) in

  let i64 = Int64.of_int_exn in *)
  (* Frenetic_NetKAT_Parser.pol_of_file Sys.argv.(1); *)
  let pol = pol_of_file Sys.argv.(1) in

  (* let topo = parse_topo_from_dotfile Sys.argv.(2) in *)
  let topo = linear (Int.of_string Sys.argv.(2)) in
  print_endline (Sexp.to_string (sexp_of_topo topo));

  let topo = tree (Int.of_string Sys.argv.(2)) (Int.of_string Sys.argv.(3)) in
  (* print_endline (Sexp.to_string (sexp_of_topo topo)); *)
  (* print_endline (Sexp.to_string (sexp_of_policy pol)); *)
  print_endline "";
  print_endline "";
  print_endline (Frenetic_NetKAT_Pretty.string_of_policy (to_local_pol (compile_local (compile pol topo))))

