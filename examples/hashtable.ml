open QCheck

module HConf =
struct
  type state = (int * string) list
  type sut   = (int, string) Hashtbl.t
  type cmd =
    | Clear
    | Add of int * string
    | Remove of int
    | Find of int
    | Find_opt of int
    | Find_all of int
    | Replace of int * string
    | Mem of int
    | Length [@@deriving show { with_path = false }]

  (*  command : int Gen.t -> string Gen.t -> command Gen.t  *)
  let command intgen strgen =
    Gen.oneof
      [ Gen.return Clear;
        Gen.map2 (fun k v -> Add (k,v)) intgen strgen;
        Gen.map  (fun k   -> Remove k) intgen;
        Gen.map  (fun k   -> Find k) intgen;
        Gen.map  (fun k   -> Find_opt k) intgen;
        Gen.map  (fun k   -> Find_all k) intgen;
        Gen.map2 (fun k v -> Replace (k,v)) intgen strgen;
        Gen.map  (fun k   -> Mem k) intgen;
        Gen.return Length; ]

  let cmdshrink c = let open Iter in match c with
    | Clear      -> Iter.empty
    | Add (k,v)  ->
      (Iter.map (fun k' -> Add (k',v)) (Shrink.int k)) <+>
      (Iter.map (fun v' -> Add (k,v')) (Shrink.string v))
    | Remove k   -> Iter.map (fun k' -> Remove k') (Shrink.int k)
    | Find k     -> Iter.map (fun k' -> Find k') (Shrink.int k)
    | Find_opt k -> Iter.map (fun k' -> Find_opt k') (Shrink.int k)
    | Find_all k -> Iter.map (fun k' -> Find_all k') (Shrink.int k)
    | Replace (k,v)  ->
      (Iter.map (fun k' -> Replace (k',v)) (Shrink.int k)) <+>
      (Iter.map (fun v' -> Replace (k,v')) (Shrink.string v))
    | Mem k      -> Iter.map (fun k' -> Mem k') (Shrink.int k)
    | Length     -> Iter.empty

  let arb_cmd s =
    let int_gen =
      if s=[]
      then Gen.small_int
      else Gen.oneof [Gen.oneofl (List.map fst s);
                      Gen.small_int] in
    QCheck.make ~print:show_cmd ~shrink:cmdshrink (command int_gen Gen.small_string)

  let init_state  = []
  let init_sut () = Hashtbl.create ~random:false 42
  let cleanup _   = ()

  let next_state c s = match c with
    | Clear         -> []
    | Add (k,v)     -> (k,v)::s
    | Remove k  -> List.remove_assoc k s
    | Replace (k,v) ->
      let rec replace h = match h with
        | [] -> [(k,v)]
        | (k',v')::h' ->
          if k = k' (*&& k<>68*) (* an arbitrary, injected bug *)
          then (k,v)::h'
          else (k',v')::replace h' in
      replace s
    | Find _
    | Find_opt _
    | Find_all _
    | Mem _
    | Length        -> s

  let run_cmd c s h = match c with
    | Clear         -> Hashtbl.clear h; true
    | Add (k,v)     -> Hashtbl.add h k v; true
    | Remove k      -> Hashtbl.remove h k; true
    | Find k        ->
      List.assoc_opt k s
        = (try Some (Hashtbl.find h k)
           with Not_found -> None)
    | Find_opt k    ->
      List.assoc_opt k s = Hashtbl.find_opt h k
    | Find_all k    ->
      let rec find_all h = match h with
        | [] -> []
        | (k',v')::h' ->
          if k = k' (*&& k<>68*) (* an arbitrary, injected bug *)
          then v'::find_all h'
          else find_all h' in
      find_all s = Hashtbl.find_all h k
    | Replace (k,v) -> Hashtbl.replace h k v; true
    | Mem k         -> List.mem_assoc k s = Hashtbl.mem h k
    | Length        -> List.length s = Hashtbl.length h

  let precond c s = true
end
module HT = QCSTM.Make(HConf)
;;
QCheck_runner.run_tests ~verbose:true [HT.agree_test ~count:10_000 ~name:"Hashtbl-model agreement"]
