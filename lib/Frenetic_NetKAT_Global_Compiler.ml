open Core.Std
open Frenetic_NetKAT

(* Internal Policy representation. Hash-consed modulo ACI. *)
module Pol = struct

  module Set = Int.Set
  module Map = Int.Map

  type policy =
    | Filter of header_val
    | Filter_out of header_val (* negated filter *)
    | Mod of header_val
    | Union of Set.t
    | Seq of int list
    | Star of int
    | Dup (* we can handle all of NetKAT *)
    (* | Deriv of FDK.t * FDK.t *)
  with sexp

  let compare p1 p2 = match p1,p2 with
    | Filter hv1, Filter hv2
    | Filter_out hv1, Filter_out hv2
    | Mod hv1, Mod hv2 -> Pervasives.compare hv1 hv2
    | Union ps1, Union ps2 -> Set.compare ps1 ps2
    | Seq ps1, Seq ps2 -> List.compare Int.compare ps1 ps2
    | Star p1, Star p2 -> Int.compare p1 p2
    | Dup, Dup -> 0
    (* | Deriv (e0,d0), Deriv (e1,d1) -> Pervasives.compare (e0,d0) (e1,d1) *)
    | Filter _, _ -> -1
    | _, Filter _ -> 1
    | Filter_out _, _ -> -1
    | _, Filter_out _ -> 1
    | Mod _, _ -> -1
    | _, Mod _ -> 1
    | Union _, _ -> -1
    | _, Union _ -> 1
    | Seq _, _ -> -1
    | _, Seq _ -> 1
    | Star _, _ -> -1
    | _, Star _ -> 1
    (* | Dup, _ -> -1 *)
    (* | _, Dup -> 1 *)

  module T = Frenetic_Hashcons.Make(struct
    type t = policy with sexp
    let compare = compare
    let hash = Hashtbl.hash (* TODO: is this ok? union are sets *)
  end)

  type t = int with sexp
  let get = T.get
  let unget = T.unget

  let drop = get (Union Set.empty)
  let id = get (Seq [])
  let dup = get Dup

  let mk_filter hv = get (Filter hv)
  let mk_filter_out hv = get (Filter_out hv)
  let mk_mod hv = get (Mod hv)

  let mk_union p1 p2 =
    match unget p1, Set.singleton p1, unget p2, Set.singleton p2 with
    | Union ps1, _, Union ps2, _
    | Union ps1, _, _, ps2
    | _, ps1, Union ps2, _
    | _, ps1, _, ps2 ->
    let ps = Set.union ps1 ps2 in
    if Set.length ps = 1 then
      match Set.to_list ps with
      | [p] -> p
      | _ -> assert false
    else
      get (Union ps)

  let mk_seq p1 p2 =
    if p1 = drop || p2 = drop then drop else
    match unget p1, [p1], unget p2, [p2] with
    | Seq pl1, _, Seq pl2, _
    | Seq pl1, _, _, pl2
    | _, pl1, Seq pl2, _
    | _, pl1, _, pl2 ->
      match pl1@pl2 with
      | [p] -> p
      | pl -> get (Seq pl)

  let mk_or = mk_union
  let mk_and = mk_seq

  let mk_big_union = List.fold ~init:drop ~f:mk_union
  let mk_big_union' = Set.fold ~init:drop ~f:mk_union
  let mk_big_seq = List.fold_left ~init:id ~f:mk_seq

  let rec mk_star p =
    if p = drop || p = id then id else
    match unget p with
    | Star p -> mk_star p
    | Filter _
    | Filter_out _
    | Mod _ -> mk_union id p
    | x -> Star p |> get

  let rec of_pred ?(negate = false) (pred : Frenetic_NetKAT.pred) : t =
    match pred with
    | True when negate -> drop
    | True -> id
    | False when negate -> id
    | False -> drop
    | Test hv when negate -> mk_filter_out hv
    | Test hv -> mk_filter hv
    | And (p1, p2) when negate -> mk_or (of_pred ~negate p1) (of_pred ~negate p2)
    | And (p1, p2) -> mk_and (of_pred p1) (of_pred p2)
    | Or (p1, p2) when negate -> mk_and (of_pred ~negate p1) (of_pred ~negate p2)
    | Or (p1, p2) -> mk_or (of_pred p1) (of_pred p2)
    | Neg pred -> of_pred ~negate:(not negate) pred

  let match_loc sw pt =
    let t1 = mk_filter (Switch sw) in
    let t2 = mk_filter (Location (Physical pt)) in
    mk_seq t1 t2

  let mk_link ?(ing : Frenetic_NetKAT.pred option) s1 p1 s2 p2 =
    (* SJS: This is not the true sematnics of a link! This is a hack that works for now,
       but we will need to use the correct encoding once we start doing things like global
       optimization or deciding equivalence. *)
    let post_link = match ing with
      | None -> match_loc s2 p2
      | Some ing -> mk_seq (mk_filter (Switch s2)) (of_pred ~negate:true ing)
    in
    mk_big_seq [match_loc s1 p1; dup; post_link ]

  let rec of_pol ?(ing : Frenetic_NetKAT.pred option) (pol : Frenetic_NetKAT.policy) : t =
    match pol with
    | Filter a -> of_pred a
    | Mod hv -> mk_mod hv
    | Union (p,q) -> mk_union (of_pol ?ing p) (of_pol ?ing q)
    | Seq (p,q) -> mk_seq (of_pol ?ing p) (of_pol ?ing q)
    | Star p -> mk_star (of_pol ?ing p)
    | Link (s1,p1,s2,p2) -> mk_link ?ing s1 p1 s2 p2
    | VLink _ -> assert false (* SJS / JNF *)

  let rec to_string (t : t) : string =
    let of_hv hv rel =
      let f,v = Frenetic_Fdd.Pattern.of_hv hv in
      let f = Frenetic_Fdd.Field.to_string f in
      let v = Frenetic_Fdd.Value.to_string v in
      sprintf "%s%s%s" f rel v
    in
    if t = id then "1" else
    if t = drop then "0" else
    match unget t with
    | Filter hv -> of_hv hv "="
    | Filter_out hv -> of_hv hv "!="
    | Mod hv -> of_hv hv ":="
    | Union ps ->
      Set.to_list ps
      |> List.map ~f:to_string
      |> String.concat ~sep:" + "
      |> sprintf "{ %s }"
    | Seq pl ->
      List.map pl ~f:to_string
      |> String.concat ~sep:"; "
      |> sprintf "[%s]"
    | Dup -> "dup"
    | Star p -> (to_string p) ^ "*"

end



(* We need some extra operations on FDKs. *)
module FDK = struct
  include Frenetic_NetKAT_Compiler.FDK

  let rec of_local_pol (pol : Pol.t) =
    match Pol.unget pol with
    | Filter hv -> of_pred (Test hv)
    | Filter_out hv -> of_pred (Neg (Test hv))
    | Mod hv -> of_mod hv
    | Union ps ->
      Pol.Set.to_list ps
      |> List.map ~f:of_local_pol
      |> List.fold ~init:drop ~f:union
    | Seq pl ->
      List.map pl ~f:of_local_pol
      |> List.fold ~init:id ~f:seq
    | Star p -> star (of_local_pol p)
    | Dup -> failwith "expected local policy, got global one"
end



(* syntactic Antimirov derivatives *)
module SynDeriv = struct

  type t = Pol.t * ((Pol.t * Pol.t) list)

  let drop = (Pol.drop, [])
  let id = (Pol.id, [])
  let dup = (Pol.drop, [(Pol.id, Pol.id)])

  let union (e1,ds1) (e2,ds2) =
    let e = Pol.mk_union e1 e2 in
    let ds = ds1 @ ds2 in
    (e, ds)

  let seq (e1,ds1) (p2, (e2,ds2)) =
    let e = Pol.mk_seq e1 e2 in
    let ds1' = List.map ds1 ~f:(fun (d,k) -> (d, Pol.mk_seq k p2)) in
    let ds2' = List.map ds2 ~f:(fun (d,k) -> (Pol.mk_seq e1 d, k)) in
    let ds = ds1' @ ds2' in
    (e, ds)

  let star p_star (e0,ds0) =
    let e = Pol.mk_star e0 in
    let ds = List.map ds0 ~f:(fun (d,k) -> (Pol.mk_seq e d, Pol.mk_seq k p_star)) in
    (e, ds)

  let rec of_pol pol =
    match Pol.unget pol with
    | Filter _
    | Filter_out _
    | Mod _ -> (pol, [])
    | Dup -> dup
    | Union ps ->
      Pol.Set.to_list ps
      |> List.map ~f:of_pol
      |> List.fold ~init:drop ~f:union
    | Seq pl ->
      List.map pl ~f:(fun p -> (p, of_pol p))
      |> List.fold ~init:id ~f:seq
    | Star p -> star pol (of_pol p)

end


(* automata states *)
module State = struct
  type t = FDK.t * FDK.t

  let to_string ?(indent="") (e,d : t) : string =
    sprintf "%sE = %s\n%sD = %s" indent (FDK.to_string e) indent (FDK.to_string d)

  let compare : t -> t -> int = Pervasives.compare

  let zero = (FDK.drop, FDK.drop)
  let one = (FDK.id, FDK.drop)

  let union (e1,d1) (e2,d2) : t =
    (FDK.union e1 e2, FDK.union d1 d2)

  let conts (e,d : t) : Int.Set.t = FDK.conts d

  let map_conts (e,d : t) ~(f:int -> int) : t =
    (e, FDK.map_conts d ~f)

  let of_syn_deriv (e,ds : SynDeriv.t) : t =
    let e = FDK.of_local_pol e in
    let ds =
      List.map ds ~f:(fun (d,k) -> FDK.seq (FDK.of_local_pol d) (FDK.mk_cont k))
      |> List.fold ~init:FDK.drop ~f:FDK.union
    in
    (e, ds)

  let of_pol (pol : Pol.t) =
    of_syn_deriv (SynDeriv.of_pol pol)

  let determinize (e,d : t) : t =
    let determinize_action par =
      let open Frenetic_NetKAT_Compiler in
      Par.to_list par
      |> List.sort ~cmp:Seq.compare_mod_k
      |> List.group ~break:(fun s1 s2 -> not (Seq.equal_mod_k s1 s2))
      |> List.map ~f:(function
        | [] -> assert false
        | [seq] -> seq
        | (rep::_ as group) ->
          let k =
            List.map group ~f:(fun s -> Seq.find_exn s K |> Value.to_int_exn)
            |> Pol.mk_big_union
          in
          Seq.add rep ~key:K ~data:(Value.of_int k))
      |> Par.of_list
    in
    (e, FDK.dedup d |> FDK.map_r determinize_action)

end


module Auto = struct

  type stateId = int
  type state = State.t

  type t = {
    start : stateId;
    states : State.t Int.Map.t;
  }

  let to_string (t : t) : string =
    Int.Map.to_alist t.states
    |> List.map ~f:(fun (id, state) ->
        sprintf "  s%d =\n%s" id (State.to_string ~indent:"    " state))
    |> List.cons (sprintf "START STATE = %d" t.start)
    |> String.concat ~sep:"\n"
    |> sprintf "%s\n"

  let of_pol' ~(determinize : bool) (pol : Pol.t) : t =
    let start = pol in
    let rec mk_states acc stateId : State.t Int.Map.t =
      if Map.mem acc stateId then acc else
      let state = State.of_pol stateId in
      let state = if determinize then State.determinize state else state in
      let init = Map.add acc ~key:stateId ~data:state in
      let conts = State.conts state in
      Set.fold conts ~init ~f:mk_states
    in
    let states = mk_states Int.Map.empty start in
    { start; states }

  let of_pol ~(determinize : bool) (pol : Frenetic_NetKAT.policy) : t =
    of_pol' ~determinize (Pol.of_pol pol)


  let pc_unused pc fdd =
    let open Frenetic_NetKAT_Compiler in
    FDK.fold
      (Par.for_all ~f:(fun seq -> not (Seq.mem seq (F pc))))
      (fun (f,_) l r -> l && r && f<>pc)
      fdd

  let to_local ~(pc : Frenetic_Fdd.Field.t) (auto : t) : FDK.t =
    let open Frenetic_NetKAT_Compiler in
    Int.Map.fold auto.states ~init:FDK.drop ~f:(fun ~key:id ~data:(e,d) acc ->
      let _ = assert (pc_unused pc e && pc_unused pc d) in
      let d =
        FDK.map_r
          (Par.map ~f:(fun seq -> match Seq.find seq K with
            | None -> failwith "transition function must specify next state!"
            | Some data -> Seq.remove seq K |> Seq.add ~key:(F pc) ~data))
          d
      in
      let guard =
        if id = auto.start then FDK.id
        else FDK.atom (pc, Value.of_int id) Action.one Action.zero in
      let fdk = FDK.seq guard (FDK.union e d) in
      FDK.union acc fdk)

end

let default = Frenetic_NetKAT_Compiler.default_compiler_options

let compile ?(options=default) (pol : Frenetic_NetKAT.policy) : FDK.t =
  Frenetic_NetKAT_Compiler.prepare_compilation ~options pol;
  Auto.of_pol pol ~determinize:true
  |> Auto.to_local ~pc:Frenetic_Fdd.Field.Vlan
