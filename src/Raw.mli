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
  (** Nullary constructor of [Nat]. *)
  | Suc of term
  (** Unary constructor of [Nat]. *)
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_suc : bound2; }
  (** Dependent elimination form for natural numbers. *)
  | Type of int
  (** Universe hierarchy. *)
  | Annot of { tm : term; ty : term; }
  (** Type annotation. *)

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

and telescope = hypothesis list

and hypothesis_desc = Hyp of { pat : pattern; ty : ty; }

and hypothesis = hypothesis_desc Position.located

type phrase_desc =
  | Val of { name : Name.t; args : telescope; ty : ty; def : term; }
  | Eval of { def : term; }

and phrase = phrase_desc Position.located

type t = phrase list

module Build : sig
  (* The functions declared in this submodule have all their arguments labelled
     except for a trailing unit argument. This ensures that the optional [loc]
     argument is never erased, making it possible to deal with locations in a
     uniform way in {! Parse}. *)

  val pvar : ?loc:Position.t -> name:Name.t -> unit -> pattern
  val pwildcard : ?loc:Position.t -> unit -> pattern
  val var : ?loc:Position.t -> name:Name.t -> unit -> term
  val let_ : ?loc:Position.t -> def:term -> ty:ty -> body:bound1 -> unit -> term
  val pi : ?loc:Position.t -> dom:ty -> cod:bound1 -> unit -> ty
  val pi_n : ?loc:Position.t -> params:telescope -> body:ty ->
                 unit -> ty
  val arrow : ?loc:Position.t -> dom:ty -> cod:ty -> unit -> ty
  val lam : ?loc:Position.t -> param:pattern -> body:term -> unit -> term
  val lam_n : ?loc:Position.t ->
              params:pattern list ->
              body:term ->
              unit -> term
  val app : ?loc:Position.t -> func:term -> arg:term -> unit -> term
  val app_n : ?loc:Position.t -> func:term -> args:term list -> unit -> term
  val sigma : ?loc:Position.t -> base:ty -> total:bound1 -> unit -> ty
  val sigma_n : ?loc:Position.t -> params:telescope -> body:ty ->
                 unit -> ty
  val product : ?loc:Position.t -> left:ty -> right:ty -> unit -> ty
  val pair : ?loc:Position.t -> left:term -> right:term -> unit -> term
  val fst : ?loc:Position.t -> arg:term -> unit -> term
  val snd : ?loc:Position.t -> arg:term -> unit -> term
  val nat : ?loc:Position.t -> unit -> term
  val zero : ?loc:Position.t -> unit -> term
  val suc : ?loc:Position.t -> t:term -> unit -> term
  val lit : ?loc:Position.t -> k:int -> unit -> term
  val natelim : ?loc:Position.t ->
                scrut:term ->
                motive:bound1 ->
                case_zero:term ->
                case_suc:bound2 ->
                unit -> term
  val typ : ?loc:Position.t -> level:int -> unit -> term
  val annot : ?loc:Position.t -> tm:term -> ty:ty -> unit -> term
  val hypothesis : ?loc:Position.t -> pat:pattern -> ty:ty -> unit -> hypothesis
  val bound1 : pattern -> term -> bound1
  val bound2 : pattern -> pattern -> term -> bound2
  val val_ : ?loc:Position.t ->
             name:Name.t ->
             args:telescope ->
             ty:term ->
             def:term ->
             unit -> phrase
  val eval : ?loc:Position.t -> def:term -> unit -> phrase
end

module PPrint : sig
  val pattern_desc : pattern_desc -> PPrint.document
  val pattern : pattern -> PPrint.document
  val term_desc : term_desc -> PPrint.document
  val term : term -> PPrint.document
  val bound1 : bound1 -> PPrint.document
  val bound2 : bound2 -> PPrint.document
  val phrase : phrase -> PPrint.document
  val hypothesis : hypothesis -> PPrint.document
  val telescope : telescope -> PPrint.document
  val file : t -> PPrint.document
end

val name_option_of_pattern : pattern -> Name.t option

(** [name_of_pattern p] sends [PWildcard] to [Name.dummy] and [PVar x] to
    [x]. *)
val name_of_pattern : pattern -> Name.t

val patterns_of_telescope : telescope -> pattern list
