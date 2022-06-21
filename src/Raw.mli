(** {1 Raw terms} *)

(** This module defines raw terms, as produced by the parser. *)

(** At this level, names are simply strings. The transformation from strings to
    DeBruijn indices happens during elaboration. *)

type name = string

type pattern_desc =
  | PWildcard
  | PVar of name

and pattern = pattern_desc Position.located

type 'a weakened = pattern * 'a

type term_desc =
  | Var of name
  (** Variable occurence *)
  | Lam of term weakened
  (** Anonymous (dependent) function *)
  | App of term * term
  (** Application *)
  | Forall of ty * ty weakened
  (** Dependent function type *)
  | Let of { bound : term;
             ty : ty;
             body : term weakened; }
  (** Let statement, annotated with its type. *)
  | Type
  (** Universe of small types. *)
  | Nat
  (** Type of natural numbers. *)
  | Zero
  (** Nullary constructor of [Nat]. *)
  | Succ
  (** Unary constructor of [Nat]. *)
  | Natelim of { discr : term;
                 motive : term weakened option;
                 case_zero : term;
                 case_succ : term weakened; }
                 (** Dependent elimination form for natural numbers. *)

and term = term_desc Position.located

and ty = term

type phrase =
  | Val of name * ty * term

type t = phrase list

val pp_term_desc : term_desc Sigs.Formatter.t

val pp_term : term Sigs.Formatter.t

val pp_phrase : phrase Sigs.Formatter.t

val pp : t Sigs.Formatter.t

module Build : sig
  val lambda : pattern list -> term -> term
  val forall : (pattern * ty) list -> ty -> ty
  val arrow : ty -> ty -> ty
end

module PPrint : sig
  val pattern_desc : pattern_desc -> PPrint.document
  val pattern : pattern -> PPrint.document
  val term_desc : term_desc -> PPrint.document
  val term : term -> PPrint.document
  val phrase : phrase -> PPrint.document
  val file : t -> PPrint.document
end
