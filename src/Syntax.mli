(** {1 Syntax} *)

(** This module defines well-typed syntax, as produced by the elaborator. *)

(** At this level, names are DeBruijn indices. *)

type name = int

type term_desc =
  | Var of name
  | Lam of bound1
  | App of term * term
  | Forall of term * bound1

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

val sexp_of_t : t -> Sexplib.Sexp.t
