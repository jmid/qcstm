open QCheck

module HanoiConf =
struct
  type peg = Peg1 | Peg2 | Peg3 [@@deriving show { with_path = false }]
  type cmd = Move of peg * peg  [@@deriving show { with_path = false }]
  type state = { peg1: int list;
                 peg2: int list;
                 peg3: int list; }
  type sut = unit

  let contents s peg = match peg with
    | Peg1 -> s.peg1
    | Peg2 -> s.peg2
    | Peg3 -> s.peg3

  let pop src s = match src with
    | Peg1 -> List.hd s.peg1, { s with peg1 = List.tl s.peg1 }
    | Peg2 -> List.hd s.peg2, { s with peg2 = List.tl s.peg2 }
    | Peg3 -> List.hd s.peg3, { s with peg3 = List.tl s.peg3 }

  let push dst s v = match dst with
    | Peg1 -> { s with peg1 = v::s.peg1 }
    | Peg2 -> { s with peg2 = v::s.peg2 }
    | Peg3 -> { s with peg3 = v::s.peg3 }

  let precond (Move (src,dst)) s =
    src <> dst &&
    match contents s src, contents s dst with
    | [], _ -> false
    | _, [] -> true
    | x::_, y::_ -> x<y

  let gen_cmd s =
    let pegs = [Peg1;Peg2;Peg3] in
    let moves =
      List.fold_right (fun src ->
          List.fold_right (fun dst acc ->
              let m = Move(src,dst) in
              if precond m s then m::acc else acc) pegs) pegs [] in
    Gen.oneofl moves

  let arb_cmd s = QCheck.make ~print:show_cmd (gen_cmd s)

  let init_state = { peg1 = [1;2;3;4];
                     peg2 = [];
                     peg3 = []; }

  let next_state (Move (src,dst)) s =
    if src = dst
    then failwith ("illegal move: " ^ show_cmd (Move (src,dst)))
    else
      let hd,s = pop src s in
      push dst s hd

  let init_sut _ = ()
  let cleanup _ = ()

  let run_cmd c s sut = (* "we never hit a state with first two pegs empty" *)
    let next = next_state c s in
    not (next.peg1 = [] && next.peg2 = [])
end

module HT = QCSTM.Make(HanoiConf)
;;
QCheck_runner.run_tests ~verbose:true [HT.agree_test ~count:100 ~name:"towers of Hanoi"]
