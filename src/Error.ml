type error =
  | Syntax of Position.t * string

let print fmt = function
  | Syntax (p, s) ->
     Format.fprintf fmt "%s: %s" (Position.to_string p) s

exception Error of error

let syntax reason startp endp =
  raise (Error (Syntax (Position.lex_join startp endp, reason)))

let syntax' reason lexbuf =
  let startp, endp = Sedlexing.lexing_positions lexbuf in
  syntax reason startp endp
