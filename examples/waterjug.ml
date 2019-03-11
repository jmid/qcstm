open QCheck

(* Example adapted from
   https://hypothesis.works/articles/how-not-to-die-hard-with-hypothesis/ and
   http://clrnd.com.ar/posts/2017-04-21-the-water-jug-problem-in-hedgehog.html *)
module WJConf =
struct
  type cmd =
    | FillBig
    | FillSmall
    | EmptyBig
    | EmptySmall
    | SmallIntoBig
    | BigIntoSmall [@@deriving show { with_path = false }]

  type state = { big : int; small : int }
  type sut = unit
      
  let arb_cmd s =
    QCheck.make ~print:show_cmd
      (Gen.oneofl [FillBig; FillSmall; EmptyBig; EmptySmall; SmallIntoBig; BigIntoSmall])

  let init_state = { big = 0; small = 0}
  let init_sut _ = ()
  let cleanup _  = ()

  let next_state c s = match c with
    | FillBig      -> { s with big = 5 }
    | FillSmall    -> { s with small = 3 }
    | EmptyBig     -> { s with big = 0 }
    | EmptySmall   -> { s with small = 0 }
    | SmallIntoBig ->
      let big' = min 5 (s.big + s.small) in
      { big = big';
        small = s.small - (big' - s.big) }
    | BigIntoSmall ->
      let small' = min 3 (s.big + s.small) in
      { big = s.big - (small' - s.small);
        small = small' }

  let run_cmd c s q = (next_state c s).big <> 4 (* s.big <> 4 *)
  let precond c s = true
end

module WJT = QCSTM.Make(WJConf)
;;
QCheck_runner.run_tests ~verbose:true [WJT.agree_test ~count:10_000 ~name:"waterjug-model"]
