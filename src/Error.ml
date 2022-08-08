type error =
  | Internal of string
  | Syntax of Position.t * string
  | Unbound_identifier of Position.t * string
  | Could_not_synthesize of Position.t
  | Unexpected_type of { loc : Position.t;
                         expected : PPrint.document;
                         actual : PPrint.document; }
  | Unexpected_head_constr of { loc : Position.t;
                                expected : [`Forall | `Univ];
                                actual : PPrint.document; }
  | Universe_inconsistency of Position.t

let print fmt = function
  | Internal message ->
     Format.fprintf fmt "internal error (%s), please open an issue at %s@\n%s"
       message
       "https://github.com/adrieng/melitte/issues"
       (Printexc.get_backtrace ())
  | Syntax (loc, s) ->
     Format.fprintf fmt "%s: %s" (Position.to_string loc) s
  | Unbound_identifier (loc, s) ->
     Format.fprintf fmt "%s: unbound identifier %s" (Position.to_string loc) s
  | Could_not_synthesize loc ->
     Format.fprintf fmt "%s: could not synthesize type (add annotation)"
       (Position.to_string loc)
  | Unexpected_type { loc; actual; expected; } ->
     Format.fprintf fmt
       "%s: this expression has type @[%a@] but type @[%a@] was expected"
       (Position.to_string loc)
       ExtPrint.pp actual
       ExtPrint.pp expected
  | Unexpected_head_constr { loc; expected; actual; } ->
     let open UnicodeSigil in
     let head_constr = function
       | `Forall -> PPrint.(doc forall ^^ space ^^ underscore)
       | `Univ -> doc typ
     in
     Format.fprintf fmt
       "%s: this expression has type @[%a@] but a type of shape @[%a@] \
        was expected"
       (Position.to_string loc)
       ExtPrint.pp actual
       (ExtPrint.to_fmt head_constr) expected
  | Universe_inconsistency loc ->
     Format.fprintf fmt "%s: universe inconsistency"
       (Position.to_string loc)

exception Error of error

let internal message =
  raise (Error (Internal message))

let syntax reason startp endp =
  raise (Error (Syntax (Position.lex_join startp endp, reason)))

let unbound_identifier loc name =
  raise (Error (Unbound_identifier (loc, name)))

let could_not_synthesize loc =
  raise (Error (Could_not_synthesize loc))

let unexpected_type ~expected ~actual loc =
  raise (Error (Unexpected_type { loc; expected; actual; }))

let unexpected_head_constr ~expected ~actual loc =
  raise (Error (Unexpected_head_constr { loc; expected; actual; }))

let universe_inconsistency loc =
  raise (Error (Universe_inconsistency loc))

