open Core
open Syntax

module Network = Frenetic_base.Network
module Net = Network.Net
module Topology = Net.Topology

type direction = Incoming | Outgoing
type topo = (abstract_location, Topology.vertex) Hashtbl.t * Topology.t

let connected_switches ~loc ~direction ~topo =
  let (name_to_vertex_table, network) = topo in
  let vertex = Hashtbl.find name_to_vertex_table loc in
  match vertex with
  | None -> failwith "no such location"
  | Some loc ->
    let neighbors = Topology.neighbors network loc in
    Topology.VertexSet.fold neighbors ~init:[] ~f:(fun acc other ->
        let node = Topology.vertex_to_label network other in
        if Network.Node.device node = Switch then
          let edges = match direction with
            | Incoming -> Topology.find_all_edges network other loc
            | Outgoing -> Topology.find_all_edges network loc other in
          Topology.EdgeSet.fold edges ~init:acc ~f:(fun acc edge ->
              let v, port =  match direction with
                | Incoming -> Topology.edge_src edge
                | Outgoing -> Topology.edge_dst edge in
              (Network.Node.id (Topology.vertex_to_label network other), port) :: acc)
        else
          acc)

let abs_loc_to_switch (topo: topo) loc =
  let (name_to_vertex_table, network) = topo in
  let vertex = Hashtbl.find name_to_vertex_table loc in
  match vertex with
  | Some v ->
    let node = (Topology.vertex_to_label network v) in
    if Network.Node.device node = Switch then
      Network.Node.id node
    else
      failwith "the location is not a switch"
  | None -> failwith "no such location"

let is_loc_host (topo: topo) loc =
  let (name_to_vertex_table, network) = topo in
  let vertex = Hashtbl.find name_to_vertex_table loc in
  match vertex with
  | Some v -> Network.Node.device (Topology.vertex_to_label network v) = Host
  | None -> failwith "no such location"

let needs_fdd_modification pol =
  let rec has_loc_or_from_test pol =
    let rec has_loc_or_from_test_pred' pred k =
      match pred with
      | True | False -> k false
      | Or (pred1, pred2)
      | And (pred1, pred2) ->
        has_loc_or_from_test_pred' pred1 (fun x ->
            if x then k x
            else has_loc_or_from_test_pred' pred2 (fun y -> k y))
      | Neg pred -> has_loc_or_from_test_pred' pred (fun x -> k x)
      | Test header -> match header with
        | AbstractLoc _ | From _ -> k true
        | _ -> k false in
    let rec has_loc_or_from_test_pol' pol k =
      match pol with
      | Union (pol1, pol2)
      | Seq (pol1, pol2) ->
        has_loc_or_from_test_pol' pol1 (fun x ->
            if x then k x
            else has_loc_or_from_test_pol' pol2 (fun y -> k y))
      | Star pol -> has_loc_or_from_test_pol' pol (fun x -> k x)
      | Dup | Link _ | VLink _ | Mod _ | Let _ -> k false
      | Filter pred -> has_loc_or_from_test_pred' pred k in
    has_loc_or_from_test_pol' pol (fun x -> x) in

  let has_loc_or_from_mod pol =
    let rec has_loc_or_from_mod' pol k =
      match pol with
      | Union (pol1, pol2)
      | Seq (pol1, pol2) ->
        has_loc_or_from_mod' pol1 (fun x ->
            if x then k x
            else has_loc_or_from_mod' pol2 (fun y -> k y))
      | Star pol -> has_loc_or_from_mod' pol (fun x -> k x)
      | Dup | Link _ | VLink _ | Filter _ | Let _ -> k false
      | Mod header -> match header with
        | AbstractLoc _ | From _ -> k true
        | x -> k false in
    has_loc_or_from_mod' pol (fun x -> x) in

  let rec needs_fdd_modification' pol k =
    match pol with
    | Union (pol1, pol2) ->
      needs_fdd_modification' pol1 (fun x ->
          if x then k x
          else needs_fdd_modification' pol2 (fun y -> k y))
    | Seq (pol1, pol2) ->
      needs_fdd_modification' pol1 (fun x ->
          if x then k x
          else needs_fdd_modification' pol2 (fun y ->
              if y then k y
              else if has_loc_or_from_mod pol1 &&
                      (has_loc_or_from_mod pol2 || has_loc_or_from_test pol2) then
                k true
              else k false))
    | Star pol -> needs_fdd_modification' pol (fun x -> if x then has_loc_or_from_test pol else k false)
    | Filter _  | Let _ | Dup  | Link _ | VLink _ | Mod _ -> k false in
  needs_fdd_modification' pol (fun x -> x)

let portify_pred pred (topo: topo) =
  let rec portify_pred' pred (k: pred -> pred) =
    match pred with
    | True -> k True
    | False -> k False
    | And (pred1, pred2) ->
      portify_pred' pred1 (fun x ->
          portify_pred' pred2 (fun y ->
              k (And (x, y))))
    | Or (pred1, pred2) ->
      portify_pred' pred1 (fun x ->
          portify_pred' pred2 (fun y ->
              k (Or (x, y))))
    | Neg pred ->
      portify_pred' pred (fun x -> k (Neg x))
    | Test header -> match header with
      | AbstractLoc loc -> k (Test (Switch (abs_loc_to_switch topo loc)))
      | From loc ->
        let from_list = connected_switches loc Incoming topo in
        List.fold from_list
          ~init:(False)
          ~f:(fun acc (sw, pt) ->
              k (Or (acc, And (
                  (Test (Switch sw)),
                  (Test (Location (Physical pt)))))))
      | Switch _ | Location _ -> failwith "cannot specify switch and port for portless policies"
      | x -> k (Test x) in
  portify_pred' pred (fun x -> x)

let portify_pol (portless_pol: policy) ~(topo: topo): policy =
  let rec portify_pol' portless_pol k =
    match portless_pol with
    | Union (pol1, pol2) ->
      portify_pol' pol1 (fun x ->
          portify_pol' pol2 (fun y ->
              k (Union (x, y))))
    | Seq (pol1, pol2) ->
      portify_pol' pol1 (fun x ->
          portify_pol' pol2 (fun y ->
              k (Seq (x, y))))
    | Star pol -> portify_pol' pol (fun x -> k (Star x))
    | Filter pred -> k (Filter (portify_pred pred topo))
    | Let meta -> portify_pol' meta.body (fun x -> k (Let {meta with body = x}))
    | Dup -> k Dup
    | Link _ | VLink _ -> failwith "links not supported for portless policies"
    | Mod header -> match header with
      | AbstractLoc loc ->
        let sw_port_list = connected_switches loc Outgoing topo in
        k (List.fold sw_port_list
             ~init:(if is_loc_host topo loc then drop else Filter (Test (Switch (abs_loc_to_switch topo loc))))
             ~f:(fun acc (sw, mod_pt) ->
                 let portful_test = Test (Switch sw) in
                 let portful_mod = Mod (Location (Physical mod_pt)) in
                 Union (acc, Seq (Filter portful_test, portful_mod))))
      | From loc -> failwith "cannot modify from"
      | Switch _ | Location _ -> failwith "cannot specify switch and port for portless policies"
      | x -> k (Mod x) in
  portify_pol' portless_pol (fun x -> x)

let make_topo (network: Topology.t): topo =
  let name_to_vertex_table = String.Table.create () in
  Topology.iter_vertexes (fun vertex ->
      let vertex_name = (Network.Node.name (Topology.vertex_to_label network vertex)) in
      let _ = Hashtbl.add name_to_vertex_table vertex_name vertex in ()) network;
  (name_to_vertex_table, network)

let compile portless_pol ~(topo: Topology.t) =
  let topo = make_topo topo in
  let policy =
    if needs_fdd_modification portless_pol then Compiler.to_local_pol (Compiler.compile_local portless_pol)
    else portless_pol in
  portify_pol policy ~topo