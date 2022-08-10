(** {2 Type declarations} *)

type value =
  | Reflect of { ty : value; tm : neutral; }
  | Lam of clo1
  | Forall of value * clo1
  | Type
  | Nat
  | Zero
  | Suc of value

and neutral

and normal = Reify of { ty : value; tm : value; }

and clo1 = C1 of env * Core.bound1

and clo2 = C2 of env * Core.bound2

and entry =
  {
    def : value;
    ty : value option;
    user : Name.t;
  }

and env = entry DeBruijn.Env.t

type ty = value

(** {2 Utility functions} *)

val close1 : Core.bound1 -> env -> clo1

val close2 : Core.bound2 -> env -> clo2

module PPrint : sig
  val value : value -> env -> PPrint.document
  val env : env -> PPrint.document
  val clo1 : clo1 -> PPrint.document
  val clo2 : clo2 -> PPrint.document
end

(** {2 Evaluation, from syntax to semantics} *)

module Eval : sig
  module M : Monad.Plain with type 'a t = env -> 'a

  val term : Core.term -> value M.t

  val clo1 : clo1 -> value -> value

  val clo2 : clo2 -> value -> value -> value
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

  val normal : normal -> Core.term M.t

  val typ : value -> Core.term M.t

  val neutral : neutral -> Core.term M.t

  (** [value] does not perform η-expansion, hence it should not be used when
      checking convertibility or, more generally, when performing normalization
      by evaluation. Nonetheless, it is useful, as it does not rely on typing
      information and can be used to print error messages in case of
      ill-typed.  *)
  val value : value -> Core.term M.t
end

(** {2 Convertibility and normalization} *)

module Conv : sig
  val ty : value -> value -> bool Quote.M.t
  val normalize : ty:ty -> tm:Core.term -> Core.term Eval.M.t
end
