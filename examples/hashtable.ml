open QCheck

module HConf =
struct
  type state = (string * int) list
  type sut   = (string, int) Hashtbl.t
  type cmd =
    | Clear
    | Add of string * int
    | Remove of string
    | Find of string
    | Find_opt of string
    | Find_all of string
    | Replace of string * int
    | Mem of string
    | Length [@@deriving show { with_path = false }]

  (*  gen_cmd : string Gen.t -> int Gen.t -> command Gen.t  *)
  let gen_cmd strgen intgen =
    Gen.oneof
      [ Gen.return Clear;
        Gen.map2 (fun k v -> Add (k,v)) strgen intgen;
        Gen.map  (fun k   -> Remove k) strgen;
        Gen.map  (fun k   -> Find k) strgen;
        Gen.map  (fun k   -> Find_opt k) strgen;
        Gen.map  (fun k   -> Find_all k) strgen;
        Gen.map2 (fun k v -> Replace (k,v)) strgen intgen;
        Gen.map  (fun k   -> Mem k) strgen;
        Gen.return Length; ]

  let shrink c = let open Iter in match c with
    | Clear      -> Iter.empty
    | Add (k,v)  ->
      (Iter.map (fun k' -> Add (k',v)) (Shrink.string k)) <+>
      (Iter.map (fun v' -> Add (k,v')) (Shrink.int v))
    | Remove k   -> Iter.map (fun k' -> Remove k') (Shrink.string k)
    | Find k     -> Iter.map (fun k' -> Find k') (Shrink.string k)
    | Find_opt k -> Iter.map (fun k' -> Find_opt k') (Shrink.string k)
    | Find_all k -> Iter.map (fun k' -> Find_all k') (Shrink.string k)
    | Replace (k,v)  ->
      (Iter.map (fun k' -> Replace (k',v)) (Shrink.string k)) <+>
      (Iter.map (fun v' -> Replace (k,v')) (Shrink.int v))
    | Mem k      -> Iter.map (fun k' -> Mem k') (Shrink.string k)
    | Length     -> Iter.empty

  let arb_cmd s =
    let str_gen =
      if s=[]
      then (Gen.small_string : string Gen.t)
      else Gen.oneof [Gen.oneofl (List.map fst s);
                      Gen.small_string] in
    QCheck.make ~print:show_cmd ~shrink:shrink (gen_cmd str_gen Gen.small_int)

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
