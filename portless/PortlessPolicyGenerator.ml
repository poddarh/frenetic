open Core
open Frenetic.Network
open Frenetic.Network.Net
open Frenetic.Netkat.Syntax

let generate_all_pairs_policy topo =
  let vertex_to_label = Topology.vertex_to_label topo in

  let rec get_policy edges to_v =
    let vertex_to_abstract_loc vertex = Node.name (vertex_to_label vertex) in
    let forwarding_pol = List.fold_left edges ~init:drop
       ~f:(fun pol edge ->
         let src_v, _ = Topology.edge_src edge in
         let dst_v, _ = Topology.edge_dst edge in
         if Node.device (Topology.vertex_to_label topo src_v) = Node.Host then
           pol
         else
           Union (pol, (Seq (Filter (Test (AbstractLoc (vertex_to_abstract_loc src_v))),
                             Mod (AbstractLoc (vertex_to_abstract_loc dst_v)))))) in
     Seq (Filter (Test (IP4Dst (Node.ip (Topology.vertex_to_label topo to_v), 32l))), forwarding_pol)
  in

  UnitPath.all_pairs_shortest_paths ~topo ~f:(fun vx1 vx2 ->
      match (Node.device (vertex_to_label vx1), Node.device (vertex_to_label vx2),
             Node.name (vertex_to_label vx1), Node.name (vertex_to_label vx2))  with
      | Node.Host, Node.Host, n1, n2 -> not (n1 = n2)
      | _ -> false)
  |> List.map ~f:(fun (w, from_v, to_v, edges) -> get_policy edges to_v)
  |> List.fold ~init:drop ~f:(fun acc p -> Union (acc, p))
(* 
let _ =
  let topo = Parse.from_dotfile Sys.argv.(1) in
  let pol = generate_all_pairs_policy topo in
  print_endline (Frenetic.Netkat.Pretty.string_of_policy pol) *)
