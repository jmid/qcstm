open QCheck
open Ctypes
open Foreign

module PGConf =
struct
  type cmd = Put of int | Get [@@deriving show { with_path = false }]
  type state = int
  type sut = Dl.library * (int -> unit) * (unit -> int)

  let arb_cmd s =
    let int_gen = Gen.oneof [Gen.map Int32.to_int int32.gen; Gen.small_int] in
    let shrink c = match c with
      | Put i -> Iter.map (fun i' -> Put i') (Shrink.int i)
      | Get   -> Iter.empty in
    QCheck.make ~print:show_cmd ~shrink:shrink
      (Gen.oneof [Gen.map (fun i -> Put i) int_gen; Gen.return Get])
      
  let init_state  = 0
  let next_state c s = match c with
    | Put i -> i
    | Get   -> s

  let init_sut () =
    let stub =
      Dl.dlopen ~filename:"_build/examples/putgetstub.so" ~flags:[Dl.RTLD_LOCAL;Dl.RTLD_NOW;] (*[Dl.RTLD_LAZY; Dl.RTLD_GLOBAL]*) in
    let put = foreign ~from:stub "put" (int @-> returning void) in
    let get = foreign ~from:stub "get" (void @-> returning int) in
    (stub,put,get)
    
  let cleanup (stub,_,_) = Dl.dlclose stub
      
  let run_cmd c s (_,put,get) = match c with
    | Put i -> (put i; true)
    | Get   -> (get () = s)

  let precond _ _ = true
end
module PGtest = QCSTM.Make(PGConf)
;;
QCheck_runner.run_tests ~verbose:true
  [PGtest.agree_test ~count:10_000 ~name:"put/get-model agreement"]
