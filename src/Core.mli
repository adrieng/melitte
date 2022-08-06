(** {1 Core Syntax} *)

(** This module defines well-typed syntax, as produced by the elaborator. *)

type term_desc =
  | Var of DeBruijn.Ix.t
  | Let of { def : term; ty : term; body : bound1; }
  | Forall of term * bound1
  | Lam of bound1
  | App of term * term
  | Nat
  | Zero
  | Suc of term
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_succ : bound2; }
  | Type

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

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.position;
  }

and t = phrase list

type ty = term

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
  val forall : ?loc:Position.t -> term -> bound1 -> term
  val lam : ?loc:Position.t -> bound1 -> term
  val app : ?loc:Position.t -> term -> term -> term
  val nat : ?loc:Position.t -> unit -> term
  val zero : ?loc:Position.t -> unit -> term
  val succ : ?loc:Position.t -> term -> term
  val natelim : ?loc:Position.t ->
                scrut:term ->
                motive:bound1 ->
                case_zero:term ->
                case_succ:bound2 ->
                unit -> term
  val typ : ?loc:Position.t -> unit -> term
  val val_ : ?loc:Position.t ->
             ?user:Name.t ->
             ty:ty ->
             body:term ->
             unit ->
             phrase
end

module PPrint : sig
  val term : term -> PPrint.document
  val phrase : phrase -> PPrint.document
  val file : t -> PPrint.document
end