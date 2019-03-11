open QCheck

module BConf =
struct
  type cmd =
    | Contents
    (* To_bytes | Sub | Blit *)
    | Nth of int
    | Length
    | Clear
    | Reset
    | Add_char of char
    (* Add_utf8_uchar | Add_utf_16le_uchar | Add_utf_16be_uchar *)
    | Add_string of string
    (*| Add_bytes of bytes*)
    | Truncate of int
  [@@deriving show { with_path = false }]

  type state = char list (* in reverse *)
  type sut = Buffer.t
      
  let arb_cmd s =
    QCheck.make ~print:show_cmd
      (Gen.oneof [Gen.return Contents;
                  Gen.map (fun i -> Nth i) Gen.small_nat;
                  Gen.return Length;
                  Gen.return Clear;
                  Gen.return Reset;
                  Gen.map (fun c -> Add_char c) Gen.char;
                  Gen.map (fun s -> Add_string s) (Gen.string);
                  Gen.map (fun i -> Truncate i) (let len = List.length s in
                                                 if len = 0
                                                 then Gen.return 0
                                                 else Gen.int_bound (len - 1));
                 ])
  
  let init_state  = []
  let init_sut () = Buffer.create 16
  let cleanup b   = Buffer.reset b

  let rev_explode s =
    let chars = ref [] in
    String.iter (fun c -> chars := c::!chars) s;
    !chars

  let explode s = List.rev (rev_explode s)
  
  let next_state c s = match c with
    | Contents -> s
    | Nth _ -> s
    | Length -> s
    | Clear -> []
    | Reset -> []
    | Add_char ch -> ch::s
    | Add_string str -> (rev_explode str)@s (*s@(explode str)*)
    | Truncate i ->
      let rec trunc buf n = match buf,n with
        | [],0 -> []
        | [],_ -> raise (Invalid_argument "truncate")
        | c::cs,0 -> []
        | c::cs,_ -> c::trunc cs (n-1) in
      List.rev (trunc (List.rev s) i)
  
  let run_cmd c s b = match c with
    | Contents -> explode (Buffer.contents b) = List.rev s
    | Nth i ->
      let r = try Some (Buffer.nth b i)
              with Invalid_argument _ -> None in
      let r' = try Some (List.nth (List.rev s) i)
               with Failure _ -> None in
      r = r'
    | Length -> (Buffer.length b) = List.length s
    | Clear -> Buffer.clear b; true
    | Reset -> Buffer.reset b; true
    | Add_char ch -> Buffer.add_char b ch; true
    | Add_string str -> Buffer.add_string b str; true
    | Truncate i ->
      try (Buffer.truncate b i; true)
      with Invalid_argument _ -> (i < 0 || i > List.length s)
      
  let precond c s = match c with
    | Truncate i -> i >= 0 && i <= List.length s
    | _ -> true
end

module BT = QCSTM.Make(BConf)
;;
QCheck_runner.run_tests ~verbose:true
  [BT.consistency_test ~count:1000 ~name:"buffer-consistent";
   BT.agree_test ~count:10_000 ~name:"buffer-model"]
