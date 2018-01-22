open Frenetic.Netkat.Parser.Portless
open Core

module Compiler = Frenetic.Netkat.Compiler
module Portless_Compiler = Frenetic.Netkat.Portless_Compiler
module Parse = Frenetic.Network.Net.Parse

let _ =
  let time = ref (Unix.gettimeofday()) in
  let print_time str =
    let now = Unix.gettimeofday() in
    print_endline (str ^ ": " ^ (Float.(to_string (now - !time))));
    time := now; in

  let topo = Parse.from_dotfile Sys.argv.(1) in
  print_time "Topology Parsed";
  let pol = PortlessPolicyGenerator.generate_all_pairs_policy topo in
  (* let pol = Frenetic.Netkat.Parser.Portless.pol_of_file Sys.argv.(2) in *)
  print_time "All-Pairs-Connected Policy Generated";
  (* let pol = Frenetic.Netkat.Syntax.(Seq(pol, Filter (Or (Test (AbstractLoc "s1"), Neg (Test (AbstractLoc "s1")))))) in *)
  let portful = Portless_Compiler.compile pol topo in
  print_time "Portless Policy compiled (Includes reordering and portification)";
  let _ = Compiler.compile_local portful in
  print_time "Portful Policy compiled";
