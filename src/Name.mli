type t = string

val equal : t -> t -> bool

val sexp_of_t : t -> Sexplib.Sexp.t

val pp : t Sigs.Formatter.t

val compare : t -> t -> int

(** A name that can never appear in source code *)
val dummy : t

(* module Env : sig *)
(*   type name = t *)
(*   type 'a t *)
(*   val width : 'a t -> int *)
(*   val empty : 'a t *)
(*   val extend : name -> 'a -> 'a t -> 'a t *)
(*   (\** [lookup name env] raises {! Not_found} whenever [name] is not bound in *)
(*       [env]. *\) *)
(*   val lookup : name -> 'a t -> DeBruijn.Ix.t * 'a *)
(*   val map : (name -> 'a -> 'b) -> 'a t -> 'b t *)
(*   val fold : (name -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
(* end *)
