open QCheck
open Ctypes
open PosixTypes
open Foreign

type stream = unit ptr
let stream : stream typ = ptr void
let stream_opt : stream option typ = ptr_opt void

let seek_set = foreign "seek_set" (void @-> returning int)
(*  FILE *fopen(const char *path, const char *mode); *)
let fopen  = foreign "fopen" (string @-> string @-> returning stream_opt)
(* int fseek(FILE *stream_pointer, long offset, int origin); *)
let fseek  = foreign "fseek" (stream @-> long @-> int @-> returning int)
(* size_t fread (void * restrict ptr, size_t size, size_t nmemb, FILE * restrict stream) *)
let fread  = foreign "fread" (ptr void @-> size_t @-> size_t @-> stream @-> returning size_t)
(* int fwrite ( const void * array, size_t size, size_t count, FILE * stream ); *)
let fwrite = foreign "fwrite" (ptr void @-> size_t @-> size_t @-> stream @-> returning int)
(* int fclose(FILE *file_pointer) *)
let fclose = foreign "fclose" (stream @-> returning int)

(* a couple of helpers, missing from the standard lib *)
let rec make_list e len = if len=0 then [] else e::(make_list e (len-1))
let rec split_list n es = match n,es with
  | 0, _     -> [],es
  | _, []    -> raise (Failure "split_list: split point beyond length")
  | _, e::es -> let fst,snd = split_list (n-1) es in
		e::fst,snd
  
(*  a simpler fread version, accepting only a number  *)
let fread' n str =
  let buf = CArray.make char n in
  let m = fread (to_voidp (CArray.start buf)) (Unsigned.Size_t.of_int 1) (Unsigned.Size_t.of_int n) str in (*(Unsigned.Size_t.to_int m), (CArray.to_list buf) *)
  let res,_ = split_list (Unsigned.Size_t.to_int m) (CArray.to_list buf) in
  res (* return the content, representing the m read entries *)

(*  a simpler fwrite version, accepting only a list  *)
let fwrite' lst str =
  let arr = CArray.of_list char lst in 
  let len = Unsigned.Size_t.of_int (List.length lst) in
  fwrite (to_voidp (CArray.start arr)) (Unsigned.Size_t.of_int 1) len str

module StdioConf = (* Reference: http://pubs.opengroup.org/onlinepubs/9699919799/ *)
struct
  type cmd =
    | Fopen of string * string
    | Fseek of int
    | Fread of int
    | Fwrite of char list
    | Fclose [@@deriving show { with_path = false }]
  type state = { status : strstatus; contents : char list; pos : int }
  and strstatus = Open | Closed | Writing | Reading
  type sut = stream option ref

  let arb_cmd s =
    let int_gen = Gen.oneof [(*Gen.map Int32.to_int int32.gen;*) Gen.small_nat] in
    let shrink c = match c with
      | Fopen (fn,flags) -> Iter.empty
      | Fread i -> Iter.map (fun i' -> Fread i') (Shrink.int i)
      | Fseek i -> Iter.map (fun i' -> Fseek i') (Shrink.int i)
      | Fwrite cs -> Iter.map (fun cs' -> Fwrite cs') (Shrink.list cs)
      | Fclose -> Iter.empty in
    QCheck.make ~print:show_cmd ~shrink:shrink
      (match s.status with
       | Closed  -> Gen.return (Fopen ("data.dat","wb+"))
       | Open    ->
	 (Gen.oneof
           [Gen.map (fun i -> Fread i) int_gen;
            Gen.map (fun i -> Fseek i) int_gen;
            Gen.map (fun cs -> Fwrite cs) (Gen.list Gen.char);
            Gen.return Fclose; ])
       | Reading ->
	 (Gen.oneof
           [Gen.map (fun i -> Fread i) int_gen;
            Gen.map (fun i -> Fseek i) int_gen;
            Gen.return Fclose; ])
       | Writing ->
	 (Gen.oneof
           [Gen.map (fun i -> Fseek i) int_gen;
	    Gen.map (fun cs -> Fwrite cs) (Gen.list Gen.char);
            Gen.return Fclose; ]))

  let init_state = { status = Closed; contents = []; pos = 0 }
  let next_state c s = match c with
    | Fopen (fn,fl) -> { init_state with status = Open } (* "wb+" truncates to 0 length *)
    | Fread i       ->
      let read = min i (max 0 (List.length s.contents - s.pos)) in
      { s with pos = s.pos + read; status = Reading} (* advance reading position *)
    | Fseek i       -> { s with pos = i; status = Open }
    | Fwrite cs     ->
      (match cs with
       | [] -> s (* If size or nitems is 0, 
		    fwrite() shall return 0 and the state of the stream remains unchanged *)
       | _  ->
	 let cs_len = List.length cs in
         let cont_len = List.length s.contents in
         let extended =
           if s.pos + List.length cs <= cont_len
           then s.contents
           else s.contents @ (make_list '\000' (s.pos + cs_len - cont_len)) in
         let pre,rest  = split_list s.pos extended in
         let _mid,post = split_list cs_len rest in
         { status   = Writing;
           contents = pre @ cs @ post;
           pos      = s.pos + cs_len })
    | Fclose        -> { s with status = Closed }

  let init_sut () = ref None
  let cleanup sut = match !sut with
    | None -> ()
    | Some str -> ignore (fclose str)
  let run_cmd c s sut = match c with
    | Fopen (fn,fl) -> let s = fopen fn fl in
      ((*Printf.printf "fopen returns %i\n%!" (Obj.obj (Obj.repr s));*)
	sut := s; s<>None)
    | Fread i       ->
      (match !sut with
       | Some str ->
	 let resbuf = fread' i str in
         if s.pos > List.length s.contents
         then resbuf = []
         else (* s.pos <= List.length s.contents *)
           let _pre,rest = split_list s.pos s.contents in
           if i > List.length rest
           then resbuf = rest
           else (* i <= List.length rest *)
	     let mid,_ = split_list i rest in
             mid = resbuf
       | None     -> raise (Failure "no stream to read"))
    | Fseek i ->
      (match !sut with
       | Some str ->
	 let s = fseek str (Signed.Long.of_int64 (Int64.of_int i)) (seek_set())
         in s=0
       | None     -> raise (Failure "no stream to seek"))
    | Fwrite cs -> (match !sut with
	| Some str -> let i = fwrite' cs str in i=(List.length cs)
        | None     -> raise (Failure "no stream to write to"))
    | Fclose -> (match !sut with
       | Some str -> let i = fclose str in
	 sut := None; i=0
       | None -> raise (Failure "no stream to close"))

  let precond c s = match c with
    | Fopen (_,_) -> s.status=Closed (* avoid reopen *)
    | Fread _  -> s.status<>Closed && s.status<>Writing
    | Fseek _  -> s.status<>Closed
    | Fwrite _ -> s.status<>Closed && s.status<>Reading
    | Fclose   -> s.status<>Closed
end
module StdioT = QCSTM.Make(StdioConf)
;;
QCheck_runner.run_tests ~verbose:true
  [StdioT.agree_test ~count:200 ~name:"stdio-model agreement"]
