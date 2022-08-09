(** The elaborator performs type-checking and elaboration of source programs, as
    represented by module {! Raw}, into the core well-scoped and well-typed
    representation, as represented by module {! Core}. This process might
    freely raise the exceptions defined in {! Error}. *)

module M : Monad.Plain

val run :
  ?on_check_pre:(Semantics.env -> expected:Semantics.ty -> Raw.term -> unit) ->
  ?on_infer_pre:(Semantics.env -> Raw.term -> unit) ->
  ?on_conversion_pre:(Semantics.env ->
                      expected:Semantics.ty -> actual:Semantics.ty ->
                      Position.t -> unit) ->
  ?on_check_post:(Semantics.env -> expected:Semantics.ty -> Raw.term -> unit) ->
  ?on_infer_post:(Semantics.env -> Raw.term -> actual:Semantics.ty -> unit) ->
  ?on_conversion_post:(Semantics.env ->
                      expected:Semantics.ty -> actual:Semantics.ty ->
                      Position.t -> unit) ->
  'a M.t ->
  'a

val check : Raw.t -> Core.t M.t
