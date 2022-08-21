(** An abstract type of name supporting constant-time equality testing,
    comparison, pretty-printing, and conversion to S-expressions. *)
include Sigs.PrintableComparableType

val sexp_of_t : t -> Sexplib.Sexp.t

(** Names are isomorphic to strings. *)

val of_string : string -> t

val to_string : t -> string

(** [dummy] is a name that can never appear in source code. *)
val dummy : t

(** [of_option name] sends [None] to [dummy] and [Some x] to [x]. *)
val of_option : t option -> t

(** [internal s] returns a name that is guaranteed never to appear in source
    code, due to lexing convention. This function is injective, but it differs
    from [of_string] in that [to_string (internal s)] is never equal to [s]. *)
val internal : string -> t
