open Core.Std
open Async.Std
open Frenetic_NetKAT_Portless
open Frenetic_NetKAT_Portless_Compiler
open Frenetic_NetKAT_Compiler
open Frenetic_NetKAT_Portless_Parser
open Tree_Topo

let _ =
  let pol = pol_of_file Sys.argv.(1) in
  let topo = match Sys.argv.(2) with
    | "tree" -> tree (Int.of_string Sys.argv.(3)) (Int.of_string Sys.argv.(4))
    | "minimal" -> minimal
    | "linear" -> linear (Int.of_string Sys.argv.(3))
    | "single" -> single (Int.of_string Sys.argv.(3))
    | _ -> failwith "Unsupported Topology" in


  print_endline "Portful Policy: \n\n";
  print_endline (Frenetic_NetKAT_Pretty.string_of_policy (to_local_pol (compile_local (compile pol topo))));
  print_endline "\n\n";

  let module Controller = Frenetic_NetKAT_Controller.Make (Frenetic_OpenFlow0x01_Plugin) in
  Controller.start 6633;
  Deferred.don't_wait_for (Controller.update (compile pol topo));
  never_returns (Scheduler.go ());