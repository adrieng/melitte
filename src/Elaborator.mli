(** The elaborator performs type-checking and elaboration of source programs, as
    represented by module {! Raw}, into the core well-scoped and well-typed
    representation, as represented by module {! Core}. This process might
    freely raise the exceptions defined in {! Error}. *)

module M : Monad.Runnable

val check : Raw.t -> Core.t M.t
