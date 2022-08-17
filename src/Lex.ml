open Parse

(** {1 Utilities} *)

let utf8_string_of_lexbuf lexbuf =
  Sigs.Unicode.utf8_string_of_uchar_array @@ Sedlexing.lexeme lexbuf

let int_of_lexbuf lexbuf =
  int_of_string @@ utf8_string_of_lexbuf lexbuf

let tabulate default table =
  let ht = Hashtbl.create 100 in
  List.iter (fun (ns, s) -> List.iter (fun n -> Hashtbl.add ht n s) ns) table;
  fun n -> try Hashtbl.find ht n with Not_found -> default n

let keyword_or_ident =
  tabulate (fun n -> ID n)
    [
      ["forall"; "∀"], FORALL;
      ["sig"; "Σ"], SIGMA;
      ["let"], LET;
      ["in"], IN;
      ["Type"; "𝕌"], TYPE;
      ["Nat"; "ℕ"], NAT;
      ["zero"], ZERO;
      ["suc"], SUC;
      ["elim"], ELIM;
      ["with"], WITH;
      ["val"], VAL;
      ["eval"], EVAL;
    ]

(** {1 Error handling} *)

let error reason lexbuf =
  let startp, endp = Sedlexing.lexing_positions lexbuf in
  Error.syntax reason startp endp

let invalid_character lexbuf =
  let reason =
    match Sedlexing.next lexbuf with
    | None -> "unknown character"
    | Some c ->
       let s = Sigs.Unicode.utf8_string_of_uchar_array [| c |] in
       Printf.sprintf "invalid character '%s'" s
  in
  error reason lexbuf

(** {1 Lexing} *)

let quark = [%sedlex.regexp? alphabetic | other_alphabetic
             | math | other_math | '_']

let atom = [%sedlex.regexp? quark, Star (quark | ascii_hex_digit)]

let nat = [%sedlex.regexp? Plus ('0' .. '9')]

let comment_start = [%sedlex.regexp? "{-"]

let comment_stop = [%sedlex.regexp? "-}"]

let rec token lexbuf = match%sedlex lexbuf with
  | white_space -> token lexbuf

  | comment_start -> comments 1 lexbuf

  | '(' -> LPAREN
  | ')' -> RPAREN
  | "{" -> LBRACE
  | '}' -> RBRACE

  | '=' -> EQ
  | "->" | 8594 -> ARR
  | "=>" | 8658 -> DARR
  | "_" -> UNDERSCORE
  | ":" -> COLON
  | "|" -> BAR
  | "," -> COMMA
  | '\\' | 955 -> LAM

  | nat -> INT (int_of_lexbuf lexbuf)
  | atom -> keyword_or_ident (utf8_string_of_lexbuf lexbuf)

  | eof -> EOF

  | _ -> invalid_character lexbuf

and comments n lexbuf =
  if n <= 0 then token lexbuf
  else
    match%sedlex lexbuf with
    | comment_start -> comments (n + 1) lexbuf
    | comment_stop -> comments (n - 1) lexbuf
    | eof -> error "unterminated comment" lexbuf
    | any -> comments n lexbuf
    | _ -> error "bad token" lexbuf
