(** {1 Raw terms} *)

(** This module defines raw terms, as produced by the parser. *)

(** At this level, names are simply strings. The transformation from strings to
    DeBruijn indices happens during elaboration. *)

type name = string [@@deriving show]

type pat = name option [@@deriving show]

type 'a weakened = pat * 'a [@@deriving show]

type term =
  | Var of name
  | Lam of term weakened
  | App of term * term
  | Forall of ty * ty weakened
  | Let of { bound : term;
             ty : ty;
             body : term weakened; }
  | Type
  | Nat | Zero | Succ
  | Natelim of { discr : term;
                 motive : term weakened option;
                 case_zero : term;
                 case_succ : term weakened; }
                 [@@deriving show]

and ty = term

type phrase =
  | Val of name * ty * term
                         [@@deriving show]

type t = phrase list [@@deriving show]
