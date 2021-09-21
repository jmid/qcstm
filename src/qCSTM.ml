(** A simple state machine framework based on QCheck *)
open QCheck


(** {1 A state machine framework for property-based testing of imperative code} *)

(** This library implements a simple, typed state machine framework
    for property-based testing of imperative code.

    It takes inspiration from the commercial Erlang state machine framework from Quviq
    and ScalaCheck's state machine framework.
*)

(** The specification of a state machine. *)
module type StmSpec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state    *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val init_state : state
  (** The model's initial state. *)

  val next_state : cmd -> state -> state
  (** Move the internal state machine to the next state. *)


  val init_sut : unit -> sut
  (** The initial state of the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val run_cmd : cmd -> state -> sut -> bool
  (** [run_cmd c s i] should interpret the command [c] over the system under test (typically side-effecting).
      [s] is in this case the model's state prior to command execution.
      The returned Boolean value should indicate whether the interpretation went well
      and in case [c] returns a value: whether the returned value agrees with the model's result. *)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c].
      This is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)
end


(** Derives a test framework from a state machine specification. *)
module Make(Spec : StmSpec) (*: StmTest *)
  : sig
    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    val arb_cmds : Spec.state -> Spec.cmd list arbitrary
    val consistency_test : ?count:int -> name:string -> Test.t
    val interp_agree : Spec.state -> Spec.sut -> Spec.cmd list -> bool
    val agree_prop : Spec.cmd list -> bool
    val agree_test : ?count:int -> name:string -> Test.t
  end
=
struct
  (** {3 The resulting test framework derived from a state machine specification} *)

  let rec gen_cmds s fuel =
    Gen.(if fuel = 0
         then return []
         else
  	  (Spec.arb_cmd s).gen >>= fun c ->
	   (gen_cmds (Spec.next_state c s) (fuel-1)) >>= fun cs ->
             return (c::cs))
  (** A fueled command list generator.
      Accepts a state parameter to enable state-dependent [cmd] generation. *)

  let rec cmds_ok s cs = match cs with
    | [] -> true
    | c::cs ->
      Spec.precond c s &&
	let s' = Spec.next_state c s in
	cmds_ok s' cs
  (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
      Accepts the initial state and the command sequence as parameters.  *)

  let arb_cmds s =
    let cmds_gen = Gen.sized (gen_cmds s) in
    let shrinker = match (Spec.arb_cmd s).shrink with
                    | None   -> Shrink.list ~shrink:Shrink.nil (* no elem. shrinker provided *)
                    | Some s -> Shrink.list ~shrink:s in
    let ac = QCheck.make ~shrink:(Shrink.filter (cmds_ok Spec.init_state) shrinker) cmds_gen in
    (match (Spec.arb_cmd s).print with
     | None   -> ac
     | Some p -> set_print (Print.list p) ac)
  (** A generator of command sequences. Accepts the initial state as parameter. *)

  let consistency_test ?(count=1000) ~name =
    Test.make ~name:name ~count:count (arb_cmds Spec.init_state) (cmds_ok Spec.init_state)
  (** A consistency test that generates a number of [cmd] sequences and
      checks that all contained [cmd]s satisfy the precondition [precond].
      Accepts an optional [count] parameter and a test name as a labeled parameter [name]. *)

  let rec interp_agree s sut cs = match cs with
    | [] -> true
    | c::cs ->
      let b = Spec.run_cmd c s sut in
      let s' = Spec.next_state c s in
      b && interp_agree s' sut cs
  (** Checks agreement between the model and the system under test
      (stops early, thanks to short-circuit Boolean evaluation). *)

  let agree_prop =
    (fun cs ->
       assume (cmds_ok Spec.init_state cs);
       let sut = Spec.init_sut () in (* reset system's state *)
       let res = interp_agree Spec.init_state sut cs in
       let ()  = Spec.cleanup sut in
       res)
  (** The agreement property: the command sequence [cs] yields the same observations
      when interpreted from the model's initial state and the [sut]'s initial state.
      Cleans up after itself by calling [Spec.cleanup] *)

  let agree_test ?(count=1000) ~name =
    Test.make ~name:name ~count:count (arb_cmds Spec.init_state) agree_prop
  (** An actual agreement test (for convenience). Accepts an optional count parameter
      and a test name as a labeled parameter [name]. *)
 end
