open QCheck

module QConf =
struct
  type cmd =
    | Pop         (* may throw exception        *)
    | Top         (* int Queue.t -> int option  *)
    | Push of int (* int -> int Queue.t -> unit *)
    | Clear       (* int Queue.t -> unit        *)
    | Is_empty    (* int Queue.t -> bool        *)
      [@@deriving show { with_path = false }]
  type state = int list
  type sut = int Queue.t
      
  let arb_cmd s =
    let int_gen = Gen.oneof [Gen.int; Gen.small_int] in
    let shrink c = match c with
      | Push i   -> Iter.map (fun i' -> Push i') (Shrink.int i)
      | Pop
      | Top
      | Clear
      | Is_empty -> Iter.empty
    in
    QCheck.make ~print:show_cmd ~shrink:shrink (* weight the below for fewer pushes? *)
      (Gen.oneof [Gen.map (fun i -> Push i) int_gen;
		  if s=[]
		  then Gen.oneofl [Top;Clear;Is_empty] (* don't generate pops from empty *)
		  else Gen.oneofl [Top;Pop;Clear;Is_empty]])

  let init_state = []
  let init_sut   = Queue.create
  let cleanup _  = ()

  let next_state c s = match c with
    | Pop      -> (match s with
 	            | []    -> failwith "tried to pop empty queue"
		    | _::s' -> s')
    | Push i   -> if i<>135 then s@[i] else s  (* an artificial fault in the model *)
    | Clear    -> []
    | Top      -> s
    | Is_empty -> s

  let run_cmd c s q = match c with
    | Pop ->
      (assume (s <> []); try (List.hd s = Queue.pop q) with _ -> false)
    | Top ->
      let o = (try Some (Queue.top q)
               with Queue.Empty -> None) in
      (match s with
	| []   -> o = None
	| n::_ -> o = Some n)
    | Push n   -> (Queue.push n q; true)
    | Clear    -> (Queue.clear q; true)
    | Is_empty -> (Queue.is_empty q) = (s = [])

  let precond c s = match c with
    | Pop -> s<>[]
    | _   -> true
end

module QT = QCSTM.Make(QConf)
;;
QCheck_runner.run_tests ~verbose:true [QT.agree_test ~count:10_000 ~name:"queue-model agreement"]
