(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
(*                                                                            *)
(*  A simple pilot for counterfactual resimulation                            *)
(*  Author / Maintainer: Jonathan Laurent (jonathan.laurent@cs.cmu.edu)       *)
(*                                                                            *)
(******************************************************************************)

open Resimulation
open Yojson

(******************************************************************************)
(* Command-line interface                                                     *)
(******************************************************************************)

let usage =
  Sys.argv.(0) ^
  " TRACE_FILE -b RULE_TO_BLOCK"

let trace_file = ref ""

let output = ref ""

let stats_file = ref ""

let max_consecutive_null = ref None

let silent = ref false

let no_specialized_instances = ref false


type intervention_trigger =
  | Rule of string

type intervention_type =
  | Punctual | Knock_instance | Knock_instances | Knock_rule

let intervention_descrs = ref []

let register trigger descr =
  intervention_descrs := !intervention_descrs @ [(trigger, descr)]

let options = [
  "--block-ponctual", Arg.String (fun r -> register (Rule r) Punctual),
  "rule to block punctually" ;
  "--block-instance", Arg.String (fun r -> register (Rule r) Knock_instance),
  "rule to block" ;
  "--block-instances", Arg.String (fun r -> register (Rule r) Knock_instances),
  "rule to block" ;
  "--block-rule", Arg.String (fun r -> register (Rule r) Knock_rule),
  "rule to block" ;
  "-o", Arg.Set_string output,
  "output file for the counterfactual trace" ;
  "--max-consecutive-null", Arg.Int (fun i -> max_consecutive_null := Some i),
  "maximal number of consecutive null events" ;
  "--stats", Arg.Set_string stats_file,
  "file in which to dump statistics on resimulation" ;
  "--silent", Arg.Set silent,
  "do not dump the trace (useful for debugging)" ;
  "--no-specialized-instances", Arg.Set no_specialized_instances,
  "disable specialized instances (this may generate a lot of null events)"
]

let anon_arg f = trace_file := f


(******************************************************************************)
(* Utilities                                                                  *)
(******************************************************************************)

let rec unzip = function
  | [] -> [], []
  | (x, y) :: l ->
    let xs, ys = unzip l in
    x :: xs, y :: ys

let name_of_rule model =
  Format.asprintf "%a" (Model.print_rule ~env:model)

let rules_of_name name model =
  Model.fold_rules (fun i acc _r ->
    if name_of_rule model i = name
    then i :: acc else acc
  ) [] model

let with_file filename f =
  if filename = "" then f Format.std_formatter
  else
    let oc = open_out filename in
    let fmt = Format.formatter_of_out_channel oc in
    let v = f fmt in
    close_out oc ;
    v

let modified_agents actions =
  let open Instantiation in
  actions
  |> List.map (function
    | Create (ag, _) -> [Agent.id ag]
    | Mod_internal ((ag, _), _) -> [Agent.id ag]
    | Bind ((ag, _), (ag', _)) | Bind_to ((ag, _), (ag', _)) -> 
      [Agent.id ag ; Agent.id ag']
    | Free (ag, _) -> [Agent.id ag]
    | Remove ag -> [Agent.id ag]
    )
  |> List.concat
  |> List.sort_uniq compare

let tested_agents tests =
  let open Instantiation in
  tests
  |> List.map (function
    | Is_Here ag -> [Agent.id ag]
    | Has_Internal _ -> []
    | Is_Free _ -> []
    | Is_Bound _ -> []
    | Has_Binding_type _ -> []
    | Is_Bound_to _ -> []
  )
  |> List.concat
  |> List.sort_uniq compare

let tested_agents_in_pe model graph pe =
  let nav = Pattern.Env.to_navigation (Model.domain model) pe.pe_pat in
  let ag = (pe.pe_root, Edges.get_sort pe.pe_root graph) in
  match Navigation.concretize ag graph nav with
  | None -> []
  | Some nav ->
    nav 
    |> List.map (fun ((ag, _s), _arr) -> ag)
    |> List.sort_uniq compare

let opt_rule_mem optr rs =
  match optr with
  | None -> false
  | Some r -> List.mem r rs

let event_id_of_tstep step =
  match Trace.simulation_info_of_step step with
  | None -> None
  | Some infos -> Some infos.Trace.Simulation_info.story_event

let factual_event_id_of_step = function
  | Factual_happened tstep -> event_id_of_tstep tstep
  | Factual_did_not_happen (_, tstep) -> event_id_of_tstep tstep
  | Counterfactual_happened _ -> None

(******************************************************************************)
(* Interventions                                                              *)
(******************************************************************************)

let block_rule model rule_name =
  let rules_to_monitor = 
    rules_of_name rule_name model in
  let block_partial _ _ pe =
    List.mem pe.pe_rule_instance rules_to_monitor in
  let block _ e =
    opt_rule_mem e.rule_instance rules_to_monitor in
  { rules_to_monitor ; block_partial ; block }

let block_punctual_event eid =
  let rules_to_monitor = [] in
  let block_partial _ _ _ = false in
  let block _ e = e.factual_event_id = Some eid in
  { rules_to_monitor ; block_partial ; block }

let block_rule_involving_any model rule_name ags =
  let rules_to_monitor = 
    rules_of_name rule_name model in
  let block_partial model graph pe =
    List.mem pe.pe_rule_instance rules_to_monitor &&
    ags |> List.exists (fun ag ->
      List.mem ag (tested_agents_in_pe model graph pe)) in
  let block _ e =
    opt_rule_mem e.rule_instance rules_to_monitor &&
    ags |> List.exists (fun ag ->
      List.mem ag (tested_agents e.tests)) in
  { rules_to_monitor ; block_partial ; block }

let block_rule_involving_all model rule_name ags =
  let rules = rules_of_name rule_name model in
  let rules_to_monitor = [] in
  let block_partial _ _ _ = false in
  let block _model e =
    opt_rule_mem e.rule_instance rules &&
    ags |> List.for_all (fun ag ->
      List.mem ag (tested_agents e.tests)) in
  { rules_to_monitor ; block_partial ; block }


(******************************************************************************)
(* Triggering interventions                                                   *)
(******************************************************************************)

let compile_trigger = function
  | Rule rule_name ->
  fun model tstep ->
    begin match tstep with
    | Trace.Subs _ | Trace.Pert _ | Trace.Init _ 
    | Trace.Dummy _ | Trace.Obs _ -> false
    | Trace.Rule (r, _, _) -> name_of_rule model r = rule_name
    end

let after_do_nothing _ state = state

let compile_intervention (trigger, descr) =

  let trigger = compile_trigger trigger in

  let fire_once f =
    let triggered = ref false in
    let before_factual tstep state =
      if not !triggered && trigger (model state) tstep then
      begin
        triggered := true ;
        let e = event_properties_of_tstep tstep in
        let rule_name = 
          match e.rule_instance with
          | None -> assert false (* see [trigger] definition *)
          | Some r -> name_of_rule (model state) r in
        f rule_name e state
      end
      else state in
    before_factual, after_do_nothing in

  (* Punctual interventions are insanely hard to encode using
     the current interface. *)
  match descr with
  | Punctual ->
    let intervention_id = ref None in
    let factual_id = ref None in
    let triggered = ref false in
    let before_factual tstep state =
      if not !triggered && trigger (model state) tstep then
      begin
        let e = event_properties_of_tstep tstep in
        let _rule_name, eid = 
          match e.rule_instance, e.factual_event_id with
          | None, _ | _, None -> assert false
          | Some r, Some eid -> name_of_rule (model state) r, eid in
        let intervention = block_punctual_event eid in
        let id, state = add_intervention intervention state in
        intervention_id := Some id ;
        factual_id := Some eid ;
        (*Format.printf "Add (%s)\n" _rule_name ;*)
        state
      end
      else state in
    let after_step step state =
      if not !triggered && !factual_id <> None &&
         factual_event_id_of_step step = !factual_id then
      begin
        (*Format.printf "Remove\n" ;*)
        triggered := true ;
        begin match !intervention_id with
        | None -> assert false
        | Some id -> remove_intervention id state
        end
      end
      else state
    in
    before_factual, after_step
  
  | Knock_rule ->
    fire_once (fun rule_name _e state ->
      let intervention = block_rule (model state) rule_name in
      snd (add_intervention intervention state)
    )
  | Knock_instance ->
    fire_once (fun rule_name e state ->
      let ags = tested_agents e.tests in
      let intervention = 
        block_rule_involving_all (model state) rule_name ags in
      snd (add_intervention intervention state)
    )
  | Knock_instances ->
    fire_once (fun rule_name e state ->
      let ags = modified_agents e.actions in
      let intervention =
        block_rule_involving_any (model state) rule_name ags in
      snd (add_intervention intervention state)
    )

let sequence fs =
  fun x state ->
  List.fold_left (fun state f ->
    f x state
  ) state fs


(******************************************************************************)
(* Main                                                                       *)
(******************************************************************************)

(* Resimulation statistics *)

type stats = {
  n_null : int ref ;
  n_factual_proper : int ref  ;
  n_counterfactual_proper : int ref  ;
  n_common : int ref 
}

let stats_init () = {
  n_null = ref 0 ;
  n_factual_proper = ref 0 ;
  n_counterfactual_proper = ref 0 ;
  n_common = ref 0 
}

let stats_to_file f stats =
  let json = `Assoc [
    "null", `Int !(stats.n_null) ;
    "factual", `Int !(stats.n_factual_proper) ;
    "counterfactual", `Int !(stats.n_counterfactual_proper) ;
    "common", `Int !(stats.n_common) ;
    "time", `Float (Sys.time ())
  ] in
  Yojson.to_file f json



let () =
    Printexc.record_backtrace true ;
    Arg.parse options anon_arg usage ;

    let stats = stats_init () in

    let do_init state =
      state
        |> set_max_consecutive_null !max_consecutive_null 
        |> set_use_specialized_instances (not !no_specialized_instances) in

    let before, after = 
      !intervention_descrs
      |> List.map compile_intervention
      |> unzip in
    let do_before_factual = sequence before in
    let do_after_step = sequence after in

    let output = 
      if !output = "" && !silent then "/dev/null"
      else !output in
    
    with_file output (fun f ->
      let handle_step model step =
        begin match step with
          | Factual_happened _ -> incr stats.n_common
          | Factual_did_not_happen _ -> incr stats.n_factual_proper
          | Counterfactual_happened _ -> incr stats.n_counterfactual_proper
        end ;
        Format.fprintf f "%a@;" 
          (Resimulation.debug_print_resimulation_step model) step in
      let handle_null_event () = incr stats.n_null in

      Format.fprintf f "@[<v>" ;
      resimulate ~do_init ~do_before_factual ~do_after_step ~handle_step ~handle_null_event !trace_file ;
      Format.fprintf f "@]@."
    ) ;
    if !stats_file <> "" then stats_to_file !stats_file stats