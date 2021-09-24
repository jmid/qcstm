open QCheck

module StConf =
struct
  type cmd =
    | Push of char
    | Pop
    | Top
    | Clear
    (* | Copy *)
    | Is_empty
    | Length
    (* | Iter f *)
    | Fold of (string -> char -> string) fun_ * string [@printer fun fmt (f,s) -> fprintf fmt "(%s, \"%s\")" (Fn.print f) (Print.string s)]
  [@@deriving show { with_path = false }]

  type state = char list
  type sut = char Stack.t

  (*  gen_cmd : state -> cmd Gen.t *)
  let gen_cmd s =
    Gen.oneof ((if s = []
                then []
                else [Gen.return Pop;
                      Gen.return Top])
               @
               [Gen.map (fun c -> Push c) Gen.char;
                Gen.return Clear;
                Gen.return Is_empty;
                Gen.return Length;
                Gen.map2 (fun f a -> Fold (f,a))
                  (fun2 Observable.string Observable.char small_string).gen
                  Gen.small_string;
               ])

  let shrink c = match c with
    | Fold (f,a) ->
      Iter.(map (fun f' -> Fold (f',a)) (Fn.shrink f)
            <+> map (fun a' -> Fold (f,a'))  (Shrink.string a))
    | _ -> Iter.empty

  let arb_cmd s = QCheck.make ~print:show_cmd ~shrink:shrink (gen_cmd s)

  let init_state = []
  let next_state c s = match c with
    | Push e     -> e::s
    | Pop        -> List.tl s
    | Clear      -> []
    | Top
    | Is_empty
    | Length
    | Fold (_,_) -> s

  let init_sut   = Stack.create
  let cleanup _  = ()
  let run_cmd c s st = match c with
    | Push e   -> Stack.push e st; true
    | Pop      -> (try Stack.pop st = List.hd s with _ -> false)
    | Top      -> (try Stack.top st = List.hd s with _ -> false)
    | Clear    -> Stack.clear st; true
    | Is_empty -> (Stack.is_empty st) = (s = [])
    | Length   -> (Stack.length st = List.length s)
    | Fold (f,a) ->
      Stack.fold (Fn.apply f) a st
      = List.fold_left (Fn.apply f) a s(*('a'::s)*) (* artificial injected bug *)

  let precond c s = match c with
    | Pop
    | Top -> s <> []
    | Push _
    | Clear
    | Is_empty
    | Length
    | Fold (_,_) -> true
end

let _ =
  let module StT = QCSTM.Make(StConf) in
  exit @@ QCheck_runner.run_tests ~verbose:true
    [StT.consistency_test ~count:1000 ~name:"stack-consistency";
     StT.agree_test ~count:10_000 ~name:"stack-model"]
