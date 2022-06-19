(** {1 Raw terms} *)

(** This module defines raw terms, as produced by the parser. *)

(** At this level, names are simply strings. The transformation from strings to
    DeBruijn indices happens during elaboration. *)

type name = string [@@deriving show]

type pat_desc = name option [@@deriving show]

and pat = pat_desc Position.located

type 'a weakened = pat * 'a [@@deriving show]

type term_desc =
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

and term = term_desc Position.located

and ty = term

type phrase =
  | Val of name * ty * term
                         [@@deriving show]

type t = phrase list [@@deriving show]

let lam id body = Position.{ value = Lam (id, body);
                             position = join id.position body.position; }

let forall (p, a) b =
  Position.{ value = Forall (a, (p, b));
             position = join (join p.position a.position) b.position; }
