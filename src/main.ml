(* A small driver for the resimulator (for tiny traces) *)

let usage =
  Sys.argv.(0) ^
  " TRACE_FILE -b RULE_TO_BLOCK"

let blocked = ref ""

let trace_file = ref ""

let options = [
  "-b", Arg.Set_string blocked, "rule to block"
]

let anon_arg f = trace_file := f

let blocked model r _ = 
  match r with
  | None -> false
  | Some r -> Format.asprintf "%a" (Model.print_rule ~env:model) r = !blocked

let () = 
    Printexc.record_backtrace true ;
    Arg.parse options anon_arg usage ;
    let rcv_step model step = 
      Format.printf "%a@;" (Resimulation.debug_print_resimulation_step model) step in

    Format.printf "@[<v>" ;
    Resimulation.resimulate ~blocked ~rcv_step !trace_file ;
    Format.printf "@]@."