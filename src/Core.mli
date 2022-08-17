(** {1 Core Syntax} *)

(** This module defines well-typed syntax, as produced by the elaborator. *)

type term_desc =
  | Var of DeBruijn.Ix.t
  | Let of { def : term; ty : term; body : bound1; }
  | Pi of term * bound1
  | Sigma of term * bound1
  | Lam of bound1
  | App of term * term
  | Nat
  | Zero
  | Suc of term
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_suc : bound2; }
  | Type of int

and term =
  {
    t_desc : term_desc;
    t_loc : Position.position;
  }

and bound1 =
  Bound1 of {
      body : term;              (* term under binder *)
      user : Name.t option;     (* for pretty-printing only *)
    }

and bound2 =
  Bound2 of {
      body : term;              (* term under binder *)
      user1 : Name.t option;    (* for pretty-printing only *)
      user2 : Name.t option;    (* for pretty-printing only *)
    }

and phrase_desc =
  | Val of { user : Name.t option; ty : term; body : term; }
  | Eval of { body : term; ty : term; }

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.position;
  }

and t = phrase list

type ty = term

val sexp_of_term : term -> Sexplib.Sexp.t
val sexp_of_bound1 : bound1 -> Sexplib.Sexp.t
val sexp_of_bound2 : bound2 -> Sexplib.Sexp.t
val sexp_of_phrase : phrase -> Sexplib.Sexp.t
val sexp_of_t : t -> Sexplib.Sexp.t

val equal_term : term -> term -> bool

module Build : sig
  val var : ?loc:Position.t -> DeBruijn.Ix.t -> term
  val let_ : ?loc:Position.t ->
             def:term ->
             ty:term ->
             body:bound1 ->
             unit ->
             term
  val pi : ?loc:Position.t -> term -> bound1 -> term
  val sigma : ?loc:Position.t -> term -> bound1 -> term
  val lam : ?loc:Position.t -> bound1 -> term
  val app : ?loc:Position.t -> term -> term -> term
  val nat : ?loc:Position.t -> unit -> term
  val zero : ?loc:Position.t -> unit -> term
  val suc : ?loc:Position.t -> term -> term
  val natelim : ?loc:Position.t ->
                scrut:term ->
                motive:bound1 ->
                case_zero:term ->
                case_suc:bound2 ->
                unit -> term
  val typ : ?loc:Position.t -> level:int -> unit -> term
  val val_ : ?loc:Position.t ->
             ?user:Name.t ->
             ty:ty ->
             body:term ->
             unit ->
             phrase
  val eval : ?loc:Position.t ->
             ty:ty ->
             body:term ->
             unit ->
             phrase
end

module ToRaw : sig
  type env = Name.t DeBruijn.Env.t
  module M : Monad.Plain with type 'a t = env -> 'a
  val term : term -> env -> Raw.term
  val bound1 : bound1 -> Raw.bound1 M.t
  val bound2 : bound2 -> Raw.bound2 M.t
  val phrase : phrase -> (Raw.phrase * env) M.t
  val file : t -> Raw.t M.t
end

module PPrint : sig
  val file : t -> PPrint.document
end
