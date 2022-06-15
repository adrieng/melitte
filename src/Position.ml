open Lexing

type lexing_position = Lexing.position

(* let lexing_position_of_sexp p = *)
(*   [%of_sexp: string * int * int * int] p *)
(*   |> fun (pos_fname, pos_lnum, pos_bol, pos_cnum) -> *)
(*      { pos_fname; pos_lnum; pos_bol; pos_cnum } *)

(* let sexp_of_lexing_position p = *)
(*   [%sexp_of: string * int * int * int] *)
(*   (p.pos_fname, p.pos_lnum, p.pos_bol, p.pos_cnum) *)

type t =
    {
      start_p : lexing_position;
      end_p   : lexing_position
    }

type position = t

type 'a located =
    {
      value    : 'a;
      position : t;
    }

let value { value = v; _ } =
  v

let position { position = p; _ } =
  p

let destruct p =
  (p.value, p.position)

let located f x =
  f (value x)

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let with_poss p1 p2 v =
  with_pos { start_p = p1; end_p = p2 } v

let map f v =
  {
    value     = f v.value;
    position  = v.position;
  }

let iter f { value = v; _ } =
  f v

let mapd f v =
  let w1, w2 = f v.value in
  let pos = v.position in
    ({ value = w1; position = pos }, { value = w2; position = pos })

let dummy =
  {
    start_p = Lexing.dummy_pos;
    end_p   = Lexing.dummy_pos
  }

let unknown_pos v =
  {
    value     = v;
    position  = dummy
  }

let start_of_position p = p.start_p

let end_of_position p = p.end_p

let filename_of_position p =
  p.start_p.Lexing.pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let join x1 x2 =
  {
    start_p = if x1 = dummy then x2.start_p else x1.start_p;
    end_p   = if x2 = dummy then x1.end_p else x2.end_p
  }

let lex_join x1 x2 =
  {
    start_p = x1;
    end_p   = x2
  }

let join_located l1 l2 f =
  {
    value    = f l1.value l2.value;
    position = join l1.position l2.position;
  }

let string_of_lex_pos p =
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let to_string p =
  let filename = filename_of_position p in
  let l = line p.start_p in
  let c1, c2 = characters p.start_p p.end_p in
    if filename = "" then
      Printf.sprintf "Line %d, characters %d-%d" l c1 c2
    else
      Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let pos_or_undef = function
  | None -> dummy
  | Some x -> x

let cpos lexbuf =
  {
    start_p = Lexing.lexeme_start_p lexbuf;
    end_p   = Lexing.lexeme_end_p   lexbuf;
  }

let with_cpos lexbuf v =
  with_pos (cpos lexbuf) v

let string_of_cpos lexbuf =
  to_string (cpos lexbuf)

let joinf f t1 t2 =
  join (f t1) (f t2)

let ljoinf f =
  List.fold_left (fun p t -> join p (f t)) dummy

let join_located_list ls f =
  {
    value     = f (List.map (fun l -> l.value) ls);
    position  = ljoinf (fun x -> x.position) ls
  }
