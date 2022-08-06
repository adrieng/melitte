(** {1 Raw terms} *)

(** This module defines raw terms, as produced by the parser. *)

(** At this level, names are simply strings. The transformation from strings to
    DeBruijn indices happens during elaboration. *)

type pattern_desc =
  | PWildcard
  | PVar of Name.t

and pattern = pattern_desc Position.located

type term_desc =
  | Var of Name.t
  (** Variable occurence *)
  | Let of { def : term;
             ty : ty;
             body : bound1; }
  (** Let statement, annotated with its type. *)
  | Forall of ty * bound1
  (** Dependent function type *)
  | Lam of bound1
  (** Anonymous (dependent) function *)
  | App of term * term
  (** Application *)
  | Nat
  (** Type of natural numbers. *)
  | Zero
  (** Nullary constructor of [Nat]. *)
  | Suc of term
  (** Unary constructor of [Nat]. *)
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_succ : bound2; }
                 (** Dependent elimination form for natural numbers. *)
  | Type
  (** Universe of small types. *)

and term = term_desc Position.located

and bound1 =
  Bound1 of {
      pat : pattern;
      body : term;
    }

and bound2 =
  Bound2 of {
      pat1 : pattern;
      pat2 : pattern;
      body : term;
    }

and ty = term

type phrase_desc =
  | Val of { name : Name.t; ty : ty; body : term; }

and phrase = phrase_desc Position.located

type t = phrase list

module Build : sig
  val pvar : Name.t -> pattern
  val bound1 : pattern -> term -> bound1
  val bound2 : pattern -> pattern -> term -> bound2
  val var : Name.t -> term
  val lambda : pattern list -> term -> term
  val forall : (pattern * ty) list -> ty -> ty
  val succ : term -> term
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

val name_option_of_pattern : pattern -> Name.t option

(** [name_of_pattern p] sends [PWildcard] to [Name.dummy] and [PVar x] to
    [x]. *)
val name_of_pattern : pattern -> Name.t
