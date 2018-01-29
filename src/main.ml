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

let blocked = ref ""

let trace_file = ref ""

let output = ref ""

let stats_file = ref ""

let max_consecutive_null = ref None

let silent = ref false

let no_specialized_instances = ref false

let options = [
  "-b", Arg.Set_string blocked, 
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

let rules_of_name name model =
  Model.fold_rules (fun i acc _r ->
    if Format.asprintf "%a" (Model.print_rule ~env:model) i = name
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
    |> List.map (fun ((ag, s), arr) -> ag)
    |> List.sort_uniq compare

let opt_rule_mem optr rs =
  match optr with
  | None -> false
  | Some r -> List.mem r rs

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

let block_punctual_event _model eid =
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
  let block model e =
    opt_rule_mem e.rule_instance rules &&
    ags |> List.for_all (fun ag ->
      List.mem ag (tested_agents e.tests)) in
  { rules_to_monitor ; block_partial ; block }

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
      let intervention = block_rule (model state) !blocked in
      state
        |> set_max_consecutive_null !max_consecutive_null 
        |> set_use_specialized_instances (not !no_specialized_instances) 
        |> add_intervention intervention |> snd in

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
      resimulate ~do_init ~handle_step ~handle_null_event !trace_file ;
      Format.fprintf f "@]@."
    ) ;
    if !stats_file <> "" then stats_to_file !stats_file stats