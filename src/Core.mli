(** {1 Core Syntax} *)

(** This module defines well-typed syntax, as produced by the elaborator. *)

(** The type of a checkable term, or [cterm], must be provided. *)
type cterm_desc =
  | Infer of iterm
  | Let of { def : cterm; ty : cterm; body : bound1; }
  | Pi of cterm * bound1
  | Lam of bound1
  | Sigma of cterm * bound1
  | Pair of cterm * cterm
  | Nat
  | Zero
  | Suc of cterm
  | Type of int

and cterm =
  {
    c_desc : cterm_desc;
    c_loc : Position.position;
  }

(** The type of an inferrable term, or [cterm], can be computed from the term
    itself (in a given environment). *)
and iterm_desc =
  | Var of DeBruijn.Ix.t
  | App of iterm * cterm
  | Fst of iterm
  | Snd of iterm
  | Natelim of { scrut : cterm;
                 motive : bound1;
                 case_zero : cterm;
                 case_suc : bound2; }
  | Annot of { tm : cterm; ty : cterm; }

and iterm =
  {
    i_desc : iterm_desc;
    i_loc : Position.position;
  }

and bound1 =
  Bound1 of {
      body : cterm;              (* cterm under binder *)
      user : Name.t option;     (* for pretty-printing only *)
    }

and bound2 =
  Bound2 of {
      body : cterm;              (* cterm under binder *)
      user1 : Name.t option;    (* for pretty-printing only *)
      user2 : Name.t option;    (* for pretty-printing only *)
    }

and phrase_desc =
  | Val of { user : Name.t option; def : iterm; }
  | Eval of { def : iterm; }

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.position;
  }

and t = phrase list

type ty = cterm

val sexp_of_cterm : cterm -> Sexplib.Sexp.t
val sexp_of_iterm : iterm -> Sexplib.Sexp.t
val sexp_of_bound1 : bound1 -> Sexplib.Sexp.t
val sexp_of_bound2 : bound2 -> Sexplib.Sexp.t
val sexp_of_phrase : phrase -> Sexplib.Sexp.t
val sexp_of_t : t -> Sexplib.Sexp.t

val equal_cterm : cterm -> cterm -> bool
val equal_iterm : iterm -> iterm -> bool

module Build : sig
  val infer : ?loc:Position.t -> iterm -> cterm
  val let_ : ?loc:Position.t ->
             def:cterm ->
             ty:cterm ->
             body:bound1 ->
             unit ->
             cterm
  val pi : ?loc:Position.t -> cterm -> bound1 -> cterm
  val lam : ?loc:Position.t -> bound1 -> cterm
  val sigma : ?loc:Position.t -> cterm -> bound1 -> cterm
  val pair : ?loc:Position.t -> cterm -> cterm -> cterm
  val nat : ?loc:Position.t -> unit -> cterm
  val zero : ?loc:Position.t -> unit -> cterm
  val suc : ?loc:Position.t -> cterm -> cterm
  val var : ?loc:Position.t -> DeBruijn.Ix.t -> iterm
  val typ : ?loc:Position.t -> level:int -> unit -> cterm

  val app : ?loc:Position.t -> iterm -> cterm -> iterm
  val fst : ?loc:Position.t -> iterm -> iterm
  val snd : ?loc:Position.t -> iterm -> iterm
  val natelim : ?loc:Position.t ->
                scrut:cterm ->
                motive:bound1 ->
                case_zero:cterm ->
                case_suc:bound2 ->
                unit -> iterm
  val annot : ?loc:Position.t -> tm:cterm -> ty:cterm -> unit -> iterm

  val val_ : ?loc:Position.t ->
             ?user:Name.t ->
             def:iterm ->
             unit ->
             phrase
  val eval : ?loc:Position.t ->
             def:iterm ->
             unit ->
             phrase
end

module ToRaw : sig
  type env = Name.t DeBruijn.Env.t
  module M : Monad.Plain with type 'a t = env -> 'a
  val cterm : cterm -> env -> Raw.term
  val iterm : iterm -> env -> Raw.term
  val bound1 : bound1 -> Raw.bound1 M.t
  val bound2 : bound2 -> Raw.bound2 M.t
  val phrase : phrase -> (Raw.phrase * env) M.t
  val file : t -> Raw.t M.t
end

module PPrint : sig
  val file : t -> PPrint.document
end
