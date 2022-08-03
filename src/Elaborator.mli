(** The elaborator performs type-checking and elaboration of source programs, as
    represented by module {! Raw}, into the internal well-scoped and well-typed
    representation, as represented by module {! Syntax}. This process might give
    rise to an error. *)

include Monad.Runnable

type value

module Env : sig
  type 'a t
  val empty : 'a t
  val extend : Raw.pattern_desc -> 'a -> 'a t -> 'a t
  val lookup : Position.t -> string -> 'a t -> 'a
end

val nf : env:Raw.ty Env.t -> ty:Raw.ty -> Raw.term -> Syntax.term

val check : Raw.t -> Syntax.t t
