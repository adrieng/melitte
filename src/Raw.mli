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
  | Let of def * term
  (** Let statement *)
  | Pi of ty * bound1
  (** Dependent function type *)
  | Lam of bound1
  (** Anonymous (dependent) function *)
  | App of term * term
  (** Application *)
  | Sigma of ty * bound1
  (** Dependent sum type *)
  | Pair of term * term
  (** Sum constructor *)
  | Fst of term
  (** First projection *)
  | Snd of term
  (** Second projection *)
  | Nat
  (** Type of natural numbers. *)
  | Zero
  (** Nullary constructor of [Nat] *)
  | Suc of term
  (** Unary constructor of [Nat] *)
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_suc : bound2; }
  (** Dependent elimination form for natural numbers *)
  | Type of int
  (** Universe hierarchy *)

and term = term_desc Position.located

and telescope = hypothesis list

and hypothesis_desc =
  H of {
      bound : pattern;
      ty : term;
    }

and hypothesis = hypothesis_desc Position.located

and boundN =
  BoundN of {
      tele : telescope;
      body : term;
    }

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

and def =
  Def of {
      pat : pattern;
      args : telescope;
      body : annotated;
    }

and annotated =
  Ann of {
      body : term;
      ty : ty;
    }

type phrase_desc =
  | Val of def
  | Eval of { def : term; ty : ty; }

and phrase = phrase_desc Position.located

type t = phrase list

module Build : sig
  (* The functions declared in this submodule have all their arguments labelled
     except for a trailing unit argument. This ensures that the optional [loc]
     argument is never erased, making it possible to deal with locations in a
     uniform way in {! Parse}. *)

  type 'a builder = ?loc:Position.t -> unit -> 'a

  val pvar : name:Name.t -> pattern builder
  val pwildcard : pattern builder
  val var : name:Name.t -> term builder
  val let_ : def:def -> body:term -> term builder
  val pi : dom:ty -> cod:bound1 -> ty builder
  val pi_n : params:(pattern * ty) list -> body:ty -> ty builder
  val arrow : dom:ty -> cod:ty -> ty builder
  val lam : param:pattern -> body:term -> term builder
  val lam_n : params:pattern list -> body:term -> term builder
  val app : func:term -> arg:term -> term builder
  val app_n : func:term -> args:term list -> term builder
  val sigma : base:ty -> total:bound1 -> ty builder
  val sigma_n : params:(pattern * ty) list -> body:ty -> ty builder
  val product : left:ty -> right:ty -> ty builder
  val pair : left:term -> right:term -> term builder
  val fst : arg:term -> term builder
  val snd : arg:term -> term builder
  val nat : term builder
  val zero : term builder
  val suc : t:term -> term builder
  val lit : k:int -> term builder
  val natelim : scrut:term -> motive:bound1 ->
                case_zero:term -> case_suc:bound2 ->
                term builder
  val typ : level:int -> ty builder
  val hypothesis : bound:pattern -> ty:term -> hypothesis builder
  val def : pat:pattern -> args:telescope -> ty:term -> body:term ->
            def builder
  val bound1 : pattern -> term -> bound1
  val bound2 : pattern -> pattern -> term -> bound2
  val val_ : def:def -> phrase builder
  val eval : ty:term -> def:term -> phrase builder
end

module PPrint : sig
  val pattern_desc : pattern_desc -> PPrint.document
  val pattern : pattern -> PPrint.document
  val term_desc : term_desc -> PPrint.document
  val term : term -> PPrint.document
  val bound1 : bound1 -> PPrint.document
  val bound2 : bound2 -> PPrint.document
  val phrase : phrase -> PPrint.document
  val file : t -> PPrint.document
end

val name_option_of_pattern : pattern -> Name.t option

(** [name_of_pattern p] sends [PWildcard] to [Name.dummy] and [PVar x] to
    [x]. *)
val name_of_pattern : pattern -> Name.t
