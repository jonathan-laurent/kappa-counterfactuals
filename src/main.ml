(* A small driver for the resimulator (for tiny traces) *)

open Resimulation
open Yojson

let usage =
  Sys.argv.(0) ^
  " TRACE_FILE -b RULE_TO_BLOCK"

let blocked = ref ""

let trace_file = ref ""

let output = ref ""

let stats_file = ref ""

let max_consecutive_null = ref None

let options = [
  "-b", Arg.Set_string blocked, 
  "rule to block" ;
  "-o", Arg.Set_string output, 
  "output file for the counterfactual trace" ;
  "--max-consecutive-null", Arg.Int (fun i -> max_consecutive_null := Some i),
  "maximal number of consecutive null events" ;
  "--stats", Arg.Set_string stats_file,
  "file in which to dump statistics on resimulation"
]

let rules_of_name name model =
  Model.fold_rules (fun i acc _r ->
    if Format.asprintf "%a" (Model.print_rule ~env:model) i = name
    then i :: acc else acc
  ) [] model

let anon_arg f = trace_file := f

let intervention model rule_name =
  let rules_to_monitor = 
    rules_of_name rule_name model in
  let block_partial _ _ pe = 
    List.mem pe.pe_rule_instance rules_to_monitor in
  let block _ e =
    match e.rule_instance with
    | None -> false
    | Some r -> List.mem r rules_to_monitor in
  (*Printf.printf "# Rules to monitor: %d\n" 
    (List.length rules_to_monitor) ; *)
  { rules_to_monitor ; block_partial ; block }


let with_file filename f =
  if filename = "" then f Format.std_formatter
  else
    let oc = open_out filename in
    let fmt = Format.formatter_of_out_channel oc in
    let v = f fmt in
    close_out oc ;
    v

let () =
    Printexc.record_backtrace true ;
    Arg.parse options anon_arg usage ;

    let n_null = ref 0 in
    let n_factual_proper = ref 0 in
    let n_counterfactual_proper = ref 0 in
    let n_common = ref 0 in

    let do_init state =
      let intervention = intervention (model state) !blocked in
      snd (add_intervention intervention state)
      |> set_max_consecutive_null !max_consecutive_null in

    with_file !output (fun f ->
      let handle_step model step =
        begin match step with
          | Factual_happened _ -> incr n_common
          | Factual_did_not_happen _ -> incr n_factual_proper
          | Counterfactual_happened _ -> incr n_counterfactual_proper
        end ;
        Format.fprintf f "%a@;" 
          (Resimulation.debug_print_resimulation_step model) step in
      let handle_null_event () = 
        incr n_null ;
        Format.fprintf f "[N] %d@;" !n_null in
      Format.fprintf f "@[<v>" ;
      resimulate ~do_init ~handle_step ~handle_null_event !trace_file ;
      Format.fprintf f "@]@." 
    ) ;
    
    if !stats_file <> "" then begin
      let json = `Assoc [
        "null", `Int !n_null ;
        "factual", `Int !n_factual_proper ;
        "counterfactual", `Int !n_counterfactual_proper ;
        "common", `Int !n_common ;
        "time", `Float (Sys.time ())
      ] in
      Yojson.to_file !stats_file json
    end