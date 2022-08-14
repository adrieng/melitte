type t = private
  | Fin of int
  | Inf

val fin : int -> t

val inf : t

val max : t -> t -> t

val ( = ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( < ) : t -> t -> bool

val sexp_of_t : t -> Sexplib.Sexp.t

module PPrint : sig
  val level : t -> PPrint.document
end
