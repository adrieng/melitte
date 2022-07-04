type error =
  | Syntax of Position.t * string
  | Unbound_identifier of Position.t * string

let print fmt = function
  | Syntax (p, s) ->
     Format.fprintf fmt "%s: %s" (Position.to_string p) s
  | Unbound_identifier (p, s) ->
     Format.fprintf fmt "%s: unbound identifier %s" (Position.to_string p) s

exception Error of error

let syntax reason startp endp =
  raise (Error (Syntax (Position.lex_join startp endp, reason)))

let unbound_identifier pos name =
  raise (Error (Unbound_identifier (pos, name)))

