(** {1 Syntax} *)

(** This module defines well-typed syntax, as produced by the elaborator. *)

(** At this level, names are De Bruijn indices. *)

type idx = int

type term_desc =
  | Var of idx
  | Lam of bound1
  | App of term * term
  | Forall of term * bound1
  | Let of term * term * bound1
  | Type
  | Nat
  | Zero
  | Succ of term
  | Natelim of { discr : term;
                 motive : bound1;
                 case_zero : term;
                 case_succ : bound1; }

and term =
  {
    t_desc : term_desc;
    t_loc : Position.position;
  }

and bound1 =
  Bound1 of {
      body : term;              (* term under binder *)
      user : Raw.name option;   (* for pretty-printing *)
    }

and phrase_desc =
  | Val of Raw.name * term * term

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.position;
  }

and t = phrase list

type ty = term

val sexp_of_t : t -> Sexplib.Sexp.t

module Build : sig
  val lam : bound1 -> term
  val zero : term
  val succ : term -> term
end

module PPrint : sig
  val term : term -> PPrint.document
  val phrase : phrase -> PPrint.document
  val file : t -> PPrint.document
end
