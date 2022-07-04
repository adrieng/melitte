open Sexplib.Std

type name = int [@@deriving sexp_of]

type term_desc =
  | Var of name
  | Lam of bound1
  | App of term * term
  | Forall of term * bound1
  | Let of term * term * bound1
  | Type
  | Nat
  | Zero
  | Succ
  | Natelim of { discr : term;
                 motive : bound1 option;
                 case_zero : term;
                 case_succ : bound1; }

and term =
  {
    t_desc : term_desc;
    t_loc : Position.t;
  }

and bound1 =
  Bound1 of {
      body : term;
      user : Raw.name option;
    }

and phrase_desc =
  | Val of Raw.name * term * term

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.t;
  }

and t = phrase list [@@deriving sexp_of]
