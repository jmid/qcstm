open QCheck

module QConf =
struct
  type cmd =
    | Pop         (* may throw exception *)
    | Top         (* may throw exception *)
    | Push of int
    | Clear
    | Is_empty [@@deriving show { with_path = false }]
  type state = int list
  type sut = int Queue.t

  let gen_cmd s =
    let int_gen = Gen.oneof [Gen.int; Gen.nat] in
    if s = []
    then Gen.oneof  (* don't generate pop/tops from empty *)
           [Gen.map (fun i -> Push i) int_gen;
            Gen.return Clear;
            Gen.return Is_empty]
    else Gen.oneof (* weight the below for fewer pushes? *)
           [Gen.return Pop;
            Gen.return Top;
            Gen.map (fun i -> Push i) int_gen;
            Gen.return Clear;
            Gen.return Is_empty]

  let arb_cmd s =
    let shrink c = match c with
      | Push i   -> Iter.map (fun i' -> Push i') (Shrink.int i)
      | Pop
      | Top
      | Clear
      | Is_empty -> Iter.empty in
    QCheck.make ~print:show_cmd ~shrink:shrink (gen_cmd s)

  let init_state = []
  let next_state c s = match c with
    | Pop      -> (match s with
 	            | []    -> failwith "tried to pop empty queue"
		    | _::s' -> s')
    | Push i   -> (* s@[i] *)
       if i<>135 then s@[i] else s  (* an artificial fault in the model *)
    | Clear    -> []
    | Top      -> s
    | Is_empty -> s


  let init_sut   = Queue.create
  let cleanup _  = ()
  let run_cmd c s q = match c with
    | Pop      -> (try (Queue.pop q = List.hd s) with _ -> false)
    | Top      -> (try (Queue.top q = List.hd s) with _ -> false)
    | Push n   -> (Queue.push n q; true)
    | Clear    -> (Queue.clear q; true)
    | Is_empty -> (Queue.is_empty q) = (s = [])

  let precond c s = match c with
    | Pop -> s<>[]
    | Top -> s<>[]
    | _   -> true
end

let _ =
  let module QT = QCSTM.Make(QConf) in
  exit @@ QCheck_runner.run_tests ~verbose:true [QT.agree_test ~count:10_000 ~name:"queue-model agreement"]
