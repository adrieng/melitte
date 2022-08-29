(** {1 Semantics} *)

(** This module handles the normalization-by-evaluation (NbE) algorithm for the
    language defined in {! Core}. This relies on three key notions: values,
    neutrals, and normals. NbE consists in two main steps:

    - evaluation, which turns a piece of source code into a semantic value, and

    - quotation, which maps a semantic value back to source code.

    In contrast with what happens in classical evaluators for non-dependent
    languages, here evaluation is extended to deal with open terms. The
    quotation process is type-directed and guaranteed to return η-long, β-short
    normal forms.
 *)

(** {2 Type declarations} *)

(** Values are results of evaluation. Since we evaluate open terms, they include
    neutrals, which are blocked computations. We use closures to represent
    not-yet-evaluated pieces of code. *)
type value =
  | Reflect of { ty : value; tm : neutral; }
  | Lam of clo1
  | Pi of value * clo1
  | Sigma of value * clo1
  | Pair of value * value
  | Type of UniverseLevel.t
  | Nat
  | Zero
  | Suc of value

(** Neutrals are left abstract. *)
and neutral

(** A normal form is a value "reified" at some type. *)
and normal = Reify of { ty : value; tm : value; }

and clo1 = C1 of env * Core.bound1

and clo2 = C2 of env * Core.bound2

and entry =
  {
    def : value;        (** Not used during quotation. *)
    ty : value option;  (** Only used during type-checking. *)
    user : Name.t;      (** Only used during elaboration *)
  }

and env = entry DeBruijn.Env.t

type ty = value

(** {2 Evaluation, from syntax to semantics} *)

module Eval : sig
  module M : Monad.Plain with type 'a t = env -> 'a

  val cterm : Core.cterm -> value M.t

  val iterm : Core.iterm -> value M.t

  val clo1 : clo1 -> value -> value

  val clo2 : clo2 -> value -> value -> value

  val fst : value -> value
end

(** {2 Quotation, from semantics to syntax} *)

module Quote : sig
  module M : Monad.Plain

  (** [run ~eta ~free] runs the quotation monad, with the parameters controling
      the number of free variables as well as whether to perform η-expansion. *)
  val run : eta:bool -> free:int -> 'a M.t -> 'a

  (** The quotation monad has access to strictly less information than the
      evaluation monad. *)
  val lift : 'a M.t -> 'a Eval.M.t

  (** [fresh ~user ~def ty] generates a fresh name in the current
      environment. The user-supplied name [user] is optional, and so is the
      definition [def], which is otherwise replaced with a fresh variable of
      type [ty]. *)
  val fresh : ?user:Name.t -> ?def:value -> ty -> entry M.t

  val normal : normal -> Core.cterm M.t

  val typ : value -> Core.cterm M.t

  val neutral : neutral -> Core.iterm M.t

  (** [value] does not perform η-expansion, hence it should not be used when
      checking convertibility or, more generally, when performing normalization
      by evaluation. Nonetheless, it is useful, as it does not rely on typing
      information and can be used to print error messages in case of
      ill-typed.  *)
  val value : value -> Core.cterm M.t
end

(** {2 Convertibility and normalization} *)

module Conv : sig
  (** The [normalize] function composes reflection and reification to obtain
      normal forms. *)
  val normalize : ty:ty -> tm:Core.cterm -> Core.cterm Eval.M.t

  (** The [ty ~lo ~hi] tests whether [lo] is a subtype to [hi] for the subtyping
      relation induced by universe levels. Conceptually, this works by comparing
      normal forms, but the algorithm here is more efficient. *)
  val ty : lo:value -> hi:value -> bool Quote.M.t
end

(** {2 Utility functions} *)

val close1 : Core.bound1 -> env -> clo1

val close2 : Core.bound2 -> env -> clo2

(** [limtype] is the top of our universe hierarchy. It does not exist in the
    syntax but allows for a simpler formulation of the elaboration algorithm. *)
val limtype : value

(** {2 Printing} *)

module PPrint : sig
  val value : value -> env -> PPrint.document
  val env : env -> PPrint.document
  val clo1 : clo1 -> PPrint.document
  val clo2 : clo2 -> PPrint.document
end
