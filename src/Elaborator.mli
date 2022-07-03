(** The elaborator performs type-checking and elaboration of source programs, as
    represented by module {! Raw}, into the internal well-scoped and well-typed
    representation, as represented by module {! Syntax}. This process might give
    rise to an error. *)

include Monad.Runnable

val check : Raw.t -> Syntax.t t
