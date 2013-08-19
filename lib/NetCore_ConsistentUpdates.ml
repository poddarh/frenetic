open NetCore_Types

module W = NetCore_Wildcard

(* No topo info ATM, so overly heavy instrumentation here *)

let internal_policy p ver =
  Seq(Filter (Hdr {all with ptrnDlVlan = W.WildcardExact (Some ver)}), p)

let edge_policy p ver =
  Seq(Filter(Hdr {all with ptrnDlVlan = W.WildcardExact None}),
        Seq(p, Action[SwitchAction {id with outPort = Here; 
          outDlVlan = Some (None, Some ver)}]))

let prOr = List.fold_left (fun pr1 pr2 -> Or(pr1, pr2)) Nothing

let ingressPort pt = Hdr {all with ptrnInPort = pt}

let strip_policy ver switches extPorts =
  ITE(prOr (List.map (fun sw -> (And(prOr (List.map (fun p -> ingressPort (W.WildcardExact (Physical p))) (extPorts sw)), OnSwitch sw))) switches), 
        Action [SwitchAction {id with outPort = Here; outDlVlan = Some (Some ver, None)}], 
        Action [SwitchAction {id with outPort = Here}])
  
let gen_update_pols orig ver switches extPorts =
  (Seq (internal_policy orig ver, strip_policy ver switches extPorts),
   Seq (edge_policy orig ver, strip_policy ver switches extPorts))
