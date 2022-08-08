type width = int

module type DB = sig
  type t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val equal : t -> t -> bool
  val to_int : t -> int
  val fresh : free:width -> t
end

module Ix : sig
  include DB
  val shift : t -> t
end

module Lv : sig
  include DB
end

val lv_of_ix : free:width -> Ix.t -> Lv.t

val ix_of_lv : free:width -> Lv.t -> Ix.t

module Env : sig
  (** An environment, to be accessed in a last-in first-out fashion. *)
  type 'a t

  (** The number of entries in the environment. *)
  val width : 'a t -> width

  (** [well_scoped env lv] checks that the De Bruijn level [lv] is well-scoped
      in [env]. *)
  val well_scoped_lv : 'a t -> Lv.t -> bool

  (** The empty environment. *)
  val empty : 'a t

  (** Extend an environment with a new entry, considered as the latest one. *)
  val extend : 'a -> 'a t -> 'a t

  (** [lookup ix env] returns the value at De Bruijn index [ix] in [env]. It
      raises [Not_found] if [ix] is not well-scoped in [env]. *)
  val lookup : Ix.t -> 'a t -> 'a

  (** [find p env] returns the first value satisfying the predicate [p] in
      [env]. It raises [Not_found] in the absence of such a value. *)
  val find : ('a -> bool) -> 'a t -> Ix.t * 'a

  (** See {! List.map}. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** See {! List.fold_right}. *)
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** See {! List.to_seq}. *)
  val to_seq : 'a t -> 'a Seq.t

  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
end
