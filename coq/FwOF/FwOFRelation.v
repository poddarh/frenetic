Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag.
Require Import FwOF.FwOF.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module Concrete := ConcreteSemantics (Atoms).
  Import Concrete.

  Axiom topo : switchId * portId -> option (switchId * portId).

  Definition abst_state := bag (switchId * portId * packet).

  Axiom relate_controller : controller -> abst_state.

  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo (sw,pt) with
          | Some (sw',pt') => {| (sw',pt',pk) |}
          | None => {| |}
        end
    end.

  Definition select_packet_out (sw : switchId) (msg : fromController) :=
    match msg with
      | PacketOut pt pk => transfer sw (pt,pk)
      | _ => {| |}
    end.

  Axiom locate_packet_in : switchId -> portId -> packet -> 
    bag (switchId * portId * packet).

  Definition select_packet_in (sw : switchId) (msg : fromSwitch) :=
    match msg with
      | PacketIn pt pk => locate_packet_in sw pt pk
      | _ => {| |}
    end.

  Definition FlowTableSafe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      Bag.unions (map (transfer sw) forwardedPkts) <+>
      Bag.unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      Bag.unions (map (transfer sw) (abst_func sw pt pk)).

  Definition FlowTablesSafe (sws : bag switch) : Prop :=
    forall swId pts tbl inp outp ctrlm switchm,
      Mem (Switch swId pts tbl inp outp ctrlm switchm) sws ->
      FlowTableSafe swId tbl.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  Axiom ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.

  Axiom ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl === select_packet_out sw msg <+>
    relate_controller ctrl'.

  Axiom ControllerRecvRemembersPackets : forall ctrl ctrl' sw msg,
    controller_recv ctrl sw msg ctrl' ->
    relate_controller ctrl' === select_packet_in sw msg <+> 
    (relate_controller ctrl).

  (* Slightly annoying since it is defined over the entire system state.
     Stronger than needed, because it holds the other devices static.
     But, this is easy to weaken! Use simpl_multistep lemma! *)
  Axiom ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 ofLinks0,
    Mem (sw,pt,pk) (relate_controller ctrl0) ->
    exists  ofLinks10 ofLinks11 ctrl1 swTo ptTo switchmLst ctrlmLst,
      (multistep 
         step (State sws0 links0 ofLinks0 ctrl0) nil
         (State sws0 links0
                (ofLinks10 ++ 
                 (OpenFlowLink swTo switchmLst 
                  (PacketOut ptTo pk :: ctrlmLst)) ::
                 ofLinks11) 
                ctrl1)) /\
      select_packet_out swTo (PacketOut ptTo pk) = ({|(sw,pt,pk)|}).


  Definition LinkHasSrc (sws : bag switch) (link : dataLink) : Prop :=
    exists switch,
      Mem switch sws /\
      fst (src link) = swId switch /\
      In (snd (src link)) (pts switch).

  Definition LinkHasDst (sws : bag switch) (link : dataLink) : Prop :=
    exists switch,
      Mem switch sws /\
      fst (dst link) = swId switch /\
      In (snd (dst link)) (pts switch).

  Definition LinksHaveSrc (sws : bag switch) (links : list dataLink) :=
    forall link, In link links -> LinkHasSrc sws link.

  Definition LinksHaveDst (sws : bag switch) (links : list dataLink) :=
    forall link, In link links -> LinkHasDst sws link.

  Definition UniqSwIds (sws : bag switch) := AllDiff swId (Bag.to_list sws).
    
  Record concreteState := ConcreteState {
    devices : state;
    concreteState_flowTableSafety : FlowTablesSafe (switches devices);
    concreteState_consistentDataLinks : ConsistentDataLinks (links devices);
    linksHaveSrc : LinksHaveSrc (switches devices) (links devices);
    linksHaveDst : LinksHaveDst (switches devices) (links devices);
    uniqSwIds : UniqSwIds (switches devices)
  }.

  Implicit Arguments ConcreteState [].

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (devices st) obs (devices st0).

  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStepEquiv : forall st st',
      st === st' ->
      abstractStep st None st'
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).

  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        FromList (map (affixSwitch swId) (Bag.to_list inp)) <+>
        Bag.unions (map (transfer swId) (Bag.to_list outp)) <+>
        Bag.unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
        Bag.unions (map (select_packet_in swId) (Bag.to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        FromList (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        Bag.unions (map (select_packet_out sw) ctrlm) <+>
        Bag.unions (map (select_packet_in sw) switchm)
    end.


  Definition relate (st : state) : abst_state :=
    Bag.unions (map relate_switch (Bag.to_list (switches st))) <+>
    Bag.unions (map relate_dataLink (links st)) <+>
    Bag.unions (map relate_openFlowLink (ofLinks st)) <+>
    relate_controller (ctrl st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (devices st)).

End Make.

