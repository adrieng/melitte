type t = string

val equal : t -> t -> bool

val sexp_of_t : t -> Sexplib.Sexp.t

val pp : t Sigs.Formatter.t

val compare : t -> t -> int

(** A name that can never appear in source code *)
val dummy : t

(** [of_option name] sends [None] to [dummy] and [Some x] to [x]. *)
val of_option : t option -> t
