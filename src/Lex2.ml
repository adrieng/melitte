open Parse

(** {1 Utilities} *)

let utf8_string_of_uchar_array a =
  let b = Buffer.create (Array.length a) in
  Array.iter (Buffer.add_utf_8_uchar b) a;
  Buffer.contents b

let utf8_string_of_lexbuf lexbuf =
  utf8_string_of_uchar_array (Sedlexing.lexeme lexbuf)

let tabulate default table =
  let ht = Hashtbl.create 100 in
  List.iter (fun (ns, s) -> List.iter (fun n -> Hashtbl.add ht n s) ns) table;
  fun n -> try Hashtbl.find ht n with Not_found -> default n

let keyword_or_ident =
  tabulate (fun n -> ID n)
    [
      ["fun"; "λ"], LAM;
      ["forall"; "∀"], FORALL;
      ["let"], LET;
      ["in"], IN;
      ["Type"; "𝕌"], TYPE;
      ["Nat"; "N"; "ℕ"], NAT;
      ["zero"], ZERO;
      ["succ"], SUCC;
      ["elim"], ELIM;
      ["with"], WITH;
      ["val"], VAL;
    ]

(** {1 Error handling} *)

let invalid_character lexbuf =
  let s = utf8_string_of_lexbuf lexbuf in
  Error.syntax' (Printf.sprintf "invalid character '%s'" s) lexbuf

(** {1 Lexing} *)

let quark = [%sedlex.regexp? alphabetic | other_alphabetic | math | other_math]

let atom = [%sedlex.regexp? quark, Star (quark | ascii_hex_digit)]

let comment_start = [%sedlex.regexp? "{-"]

let comment_stop = [%sedlex.regexp? "-}"]

let rec token lexbuf = match%sedlex lexbuf with
  | '\n' -> Sedlexing.new_line lexbuf; token lexbuf
  | white_space -> token lexbuf

  | comment_start -> comments 1 lexbuf

  | '(' -> LPAREN
  | ')' -> RPAREN
  | "->" -> LBRACE
  | '}' -> RBRACE

  | '=' -> EQ
  | "->" | 8594 -> ARR
  | "=>" | 8658 -> DARR
  | "_" -> UNDERSCORE
  | ":" -> COLON
  | "|" -> BAR

  | atom -> keyword_or_ident (utf8_string_of_lexbuf lexbuf)

  | _ -> invalid_character lexbuf

and comments n lexbuf =
  if n <= 0 then token lexbuf
  else
    match%sedlex lexbuf with
    | '\n' -> Sedlexing.new_line lexbuf; comments n lexbuf
    | comment_start -> comments (n + 1) lexbuf
    | comment_stop -> comments (n - 1) lexbuf
    | eof -> Error.syntax' "unterminated comment" lexbuf
    | _ -> comments n lexbuf
