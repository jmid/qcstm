open QCheck
open Ctypes
open PosixTypes
open Foreign

type queue = unit ptr
let queue : queue typ = ptr void

let alloc = foreign "new" (int @-> returning queue)        (* Queue *new(int n) *)
let put = foreign "put" (queue @-> int @-> returning void) (* void put(Queue *q, int n) *)
let get = foreign "get" (queue @-> returning int)          (* int get(Queue *q) *)
let size = foreign "size" (queue @-> returning int)        (* int size(Queue *q) *)

module CqConf =
struct
  type cmd =
    | New of int
    | Put of int
    | Get
    | Size [@@deriving show { with_path = false }]

  type state = Undef | Def of { size: int; contents : int list }
  type sut = queue option ref
  let arb_cmd s =
    let int_gen = Gen.oneof [(*Gen.map Int32.to_int int32.gen;*) Gen.small_nat] in
    let shrink c = match c with
      | New i -> Iter.map (fun i' -> New i') (Shrink.int i)
      | Put i -> Iter.map (fun i' -> Put i') (Shrink.int i)
      | Get
      | Size -> Iter.empty in
    QCheck.make ~print:show_cmd ~shrink:shrink
      (match s with
       | Undef -> Gen.map (fun i -> New (i+1)) int_gen
       | Def s ->
	 Gen.oneof
           ((if s.contents <> [] then [Gen.return Get] else [])
            @
            (if List.length s.contents < s.size
             then [Gen.map (fun i -> Put i) int_gen]
             else [])
            @
            [ Gen.return Size; ]))
  let init_sut () = ref None
  let cleanup _ = ()                
  let init_state = Undef
    
  let next_state c s = match c with
    | New n -> Def { size = n; contents = [] }
    | Put n -> (match s with
                 | Undef -> raise (Failure "no model to put")
                 | Def s -> Def { s with contents = s.contents@[n] })
    | Get   -> (match s with
                 | Undef -> raise (Failure "no model to get")
                 | Def s -> Def { s with contents = List.tl s.contents })
    | Size  -> s

  let run_cmd c s q = match c with
    | New n -> q := Some (alloc n); true
    | Put n -> (match !q with
                 | Some q -> put q n; true
                 | None   -> raise (Failure "no queue to put"))
    | Get   -> (match !q with
                 | Some q ->
                   let i = get q in
                   (match s with
                    | Undef -> false
                    | Def s -> i = List.hd s.contents)
                 | None   -> raise (Failure "no queue to get"))
    | Size  -> (match !q with
                 | Some q ->
                   (match s with
                    | Undef -> ignore (size q); false
                    | Def s -> List.length s.contents = size q)
                 | None   -> raise (Failure "no queue to size"))
      
  let precond c s = match c with
    | New n -> s = Undef && n > 0
    | Put _ -> (match s with
                 | Undef -> false
                 | Def s -> List.length s.contents < s.size)
    | Get   -> (match s with
                 | Undef -> false
                 | Def s -> s.contents <> [])
    | Size  -> s <> Undef
end

module CqT = QCSTM.Make(CqConf)
;;
QCheck_runner.run_tests ~verbose:true
  [CqT.consistency_test ~count:2000 ~name:"circular-queue consistency";
   CqT.agree_test ~count:10_000 ~name:"circular-queue-model agreement"]
