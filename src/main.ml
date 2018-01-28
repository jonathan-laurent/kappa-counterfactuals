(* A small driver for the resimulator (for tiny traces) *)

open Resimulation

let usage =
  Sys.argv.(0) ^
  " TRACE_FILE -b RULE_TO_BLOCK"

let blocked = ref ""

let trace_file = ref ""

let options = [
  "-b", Arg.Set_string blocked, "rule to block"
]

let anon_arg f = trace_file := f

let blocked model {rule_instance ; _} = 
  match rule_instance with
  | None -> false
  | Some r -> Format.asprintf "%a" (Model.print_rule ~env:model) r = !blocked

let () = 
    Printexc.record_backtrace true ;
    Arg.parse options anon_arg usage ;
    let handle_step model step = 
      Format.printf "%a@;" (Resimulation.debug_print_resimulation_step model) step in

    let do_init state =
      snd (add_intervention blocked state) in

    Format.printf "@[<v>" ;
    resimulate ~do_init ~handle_step !trace_file ;
    Format.printf "@]@."