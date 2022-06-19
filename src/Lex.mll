{
  open Parse

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

  let invalid_character lexbuf =
    let s = Lexing.lexeme lexbuf in
    let startp = Lexing.lexeme_start_p lexbuf in
    let endp = Lexing.lexeme_end_p lexbuf in
    Error.syntax (Printf.sprintf "invalid character '%s'" s) startp endp
}

let atom =
  [^ '(' ')' '{' '}' '[' ']' '=' ':' '<' '>' '_' '|' '.' ' ' '\t' '\n' '\r']*

let comment_start = "{-"

let comment_stop = "-}"

rule token = parse
| [' ' '\t'] { token lexbuf }

| '\n' { Lexing.new_line lexbuf; token lexbuf }

| comment_start { comments 1 lexbuf }

| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }

| '=' { EQ }
| "->" | "→" { ARR }
| "=>" | "⇒" { DARR }
| "_" { UNDERSCORE }
| ":" { COLON }
| "|" { BAR }

| atom { keyword_or_ident (Lexing.lexeme lexbuf) }

| eof { EOF }

| _ { invalid_character lexbuf }

and comments n = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| comment_stop { if n = 1 then token lexbuf else comments (n - 1) lexbuf }
| comment_start { comments (n + 1) lexbuf }
| _ { comments n lexbuf }

{
let string_of_token = function
| ID s -> "ID(" ^ s ^")"
| LAM -> "LAM"
| FORALL -> "FORALL"
| LET -> "LET"
| IN -> "IN"
| TYPE -> "TYPE"
| NAT -> "NAT"
| ZERO -> "ZERO"
| SUCC -> "SUCC"
| ELIM -> "ELIM"
| WITH -> "WITH"
| VAL -> "VAL"
| LPAREN -> "LPAREN"
| RPAREN -> "RPAREN"
| LBRACE -> "LBRACE"
| RBRACE -> "RBRACE"
| EQ -> "EQ"
| ARR -> "ARR"
| DARR -> "DARR"
| UNDERSCORE -> "UNDERSCORE"
| COLON -> "COLON"
| BAR -> "BAR"
| EOF -> "EOF"

let debug = ref false

let token lexbuf =
  let tok = token lexbuf in
  if !debug then Format.eprintf "%s@." (string_of_token tok);
  tok
}