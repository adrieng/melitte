(** {1 Raw terms} *)

(** This module defines raw terms, as produced by the parser. *)

type raw_name = string [@@deriving show]

type term =
  | Var of raw_name
  | Lam of weakened_term
  | App of term * term
  | Forall of weakened_term
  | Let of term * weakened_term
  | Type
  | Nat
  | Zero
  | Succ of term
  | Natelim of term * (pat * ty) * term * weakened_term
                                             [@@deriving show]

and ty = term

and pat = raw_name option

and binding =
  {
    id : pat;
    ann : ty option;
  }

and weakened_term = binding * term

and weakened_ty = weakened_term

type phrase =
  | Val of binding * term
                       [@@deriving show]

type t = phrase list
           [@@deriving show]
