(** {1 Core Syntax} *)

(** This module defines well-typed syntax, as produced by the elaborator. *)

type term_desc =
  | Var of DeBruijn.Ix.t
  | Let of def * term
  | Pi of term * bound1
  | Lam of bound1
  | App of term * term
  | Sigma of term * bound1
  | Pair of term * term
  | Fst of term
  | Snd of term
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

and telescope = hypothesis list

and hypothesis =
  H of {
      user : Name.t option;
      ty : term;
      loc : Position.position;
    }

and boundN =
  BoundN of {
      tele : telescope;
      body : term;
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

and def =
  Def of {
      user : Name.t option;     (* for pretty-printing only *)
      body : annotated;
    }

and annotated =
  Ann of {
      body : term;
      ty : term;
    }

and phrase_desc =
  | Val of def
  | Eval of { def : term; ty : term; }

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
  type 'a locator = ?loc:Position.t -> 'a
  val var : (DeBruijn.Ix.t -> term) locator
  val let_ : (def -> term -> term) locator
  val pi : (term -> bound1 -> term) locator
  val lam : (bound1 -> term) locator
  val app : (term -> term -> term) locator
  val sigma : (term -> bound1 -> term) locator
  val pair : (term -> term -> term) locator
  val fst : (term -> term) locator
  val snd : (term -> term) locator
  val nat : (unit -> term) locator
  val zero : (unit -> term) locator
  val suc : (term -> term) locator
  val natelim : (scrut:term ->
                 motive:bound1 ->
                 case_zero:term ->
                 case_suc:bound2 ->
                 unit -> term) locator
  val typ : (int -> term) locator
  val val_ : (def -> phrase) locator
  val eval : (ty -> term -> phrase) locator
end

module ToRaw : sig
  type env = Name.t DeBruijn.Env.t
  module M : Monad.Plain with type 'a t = env -> 'a
  val term : term -> env -> Raw.term
  val bound1 : bound1 -> Raw.bound1 M.t
  val bound2 : bound2 -> Raw.bound2 M.t
  val boundN : boundN -> Raw.boundN M.t
  val file : t -> Raw.t M.t
end

module PPrint : sig
  val file : t -> PPrint.document
end
