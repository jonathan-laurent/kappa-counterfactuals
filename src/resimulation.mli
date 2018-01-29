(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
(*                                                                            *)
(*  Author / Maintainer: Jonathan Laurent (jonathan.laurent@cs.cmu.edu)       *)
(*                                                                            *)
(******************************************************************************)

(** Resimulation algorithm that serves as a basis for 
    counterfactual reasoning. *)

type state

type intervention_id

type step =
  | Factual_happened of Trace.step
  | Factual_did_not_happen of (intervention_id list) * Trace.step
  | Counterfactual_happened of Trace.step

val debug_print_resimulation_step : Model.t -> Format.formatter -> step -> unit

exception End_of_resimulation


type event_properties = {
  rule_instance: int option ;
  actions: (Instantiation.concrete Instantiation.action) list ;
  tests: (Instantiation.concrete Instantiation.test) list ;
  factual_event_id: int option ;
}

type event_predicate = Model.t -> event_properties -> bool

type partial_event = {
  pe_rule_instance: int ;
  pe_pat: Pattern.id ;
  pe_root: int 
}

type partial_event_predicate =
  Model.t -> Edges.t -> partial_event -> bool

type intervention = {
  block: event_predicate ;
  block_partial: partial_event_predicate ;
  rules_to_monitor: int list ;
}

val clear_interventions : state -> state

val add_intervention :
  ?timeout:float -> intervention -> state -> intervention_id * state
(** if provided, [timeout] is the time at which the 
    intervention gets deactivated *)

val remove_intervention : intervention_id -> state -> state

val model : state -> Model.t

val set_max_consecutive_null : int option -> state -> state

val set_use_specialized_instances : bool -> state -> state

val init : Model.t -> Random.State.t -> state

val do_step :
  ?do_before_factual:(Trace.step -> state -> state) ->
  Trace.step option -> state -> bool * step option * state
(** [do_step next_reference_step state]
    @return [(reference_step_consummed, counterfactual_step, new_state)]

    Performs one step of resimulation. The first argument should be
    [Some (step, blocked)] where [step] is the next step in the 
    reference trace and [block]
    is true iff it should be blocked. It should be [None] if there are no
    more events in the reference trace.
    This function returns a boolean which is
    true iff the given reference step has been consummed, an optional
    resimulation step ([None] in case of a null event) and a new state.

    The exception [End_of_resimulation] is raised if there are no
    more events in the reference trace and the divergent activity is 0. *)


val resimulate : 
  ?do_init:(state -> state) ->
  ?do_before_factual:(Trace.step -> state -> state) ->
  ?do_after_step:(step -> state -> state) ->
  ?handle_null_event:(unit -> unit) ->
  ?handle_step:(Model.t -> step -> unit) ->
  ?stop_after:(Model.t -> step -> bool) ->
  string ->
  unit