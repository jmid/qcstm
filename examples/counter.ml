open QCheck

module CConf =
struct
  type cmd =
    | Incr
    | Decr
    | Set of int
    | Deref [@@deriving show { with_path = false }]
  type state = int
  type sut = int ref

  let arb_cmd _ =
    let int_gen = Gen.oneof [Gen.int; Gen.small_int] in
    QCheck.make ~print:show_cmd
      (Gen.oneof [Gen.return Incr;
		  Gen.return Decr;
		  Gen.map (fun i -> Set i) int_gen;
		  Gen.return Deref])

  let init_state  = 0
  let init_sut () = ref 0
  let cleanup _   = ()

  let next_state c s = match c with
    | Incr  -> s+1
    | Decr  -> s-1
    | Set i -> if i<>1213 then i else s (* an artificial fault *)
    | Deref -> s

  let run_cmd c s r = match c with
    | Incr  -> (incr r; true)
    | Decr  -> (decr r; true)
    | Set i -> (r := i; true)
    | Deref -> !r = s
      
  let precond _ _ = true
end
module CT = QCSTM.Make(CConf)
;;
QCheck_runner.run_tests ~verbose:true [CT.agree_test ~count:10_000 ~name:"ref-model agreement"]
