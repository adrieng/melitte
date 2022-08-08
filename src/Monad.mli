module type Plain = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Runnable = sig
  include Plain
  val run : 'a t -> 'a
end

module Notation (M : Plain) : sig
  val return : 'a -> 'a M.t
  val (let*) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
  val (and*) : 'a M.t -> 'b M.t -> ('a * 'b) M.t
  val (let+) : 'a M.t -> ('a -> 'b) -> 'b M.t
  val (and+) : 'a M.t -> 'b M.t -> ('a * 'b) M.t
end

module type TYPE = sig
  type t
end

module Reader (T : TYPE) : sig
  include Plain with type 'a t = T.t -> 'a
  val get : T.t t
  val run : T.t -> 'a t -> 'a
end

module State(T : TYPE) : sig
  include Plain with type 'a t = T.t -> 'a * T.t
  val get : T.t t
  val set : T.t -> unit t
  val run : T.t -> 'a t -> 'a
end

module Error(T : TYPE) : sig
  include Plain
  val fail : T.t -> 'a t
  val run : 'a t -> ('a, T.t) Result.t
end

module ErrorT(T : TYPE)(M : Plain) : sig
  include Plain with type 'a t = ('a, T.t) Result.t M.t
  val lift : 'a M.t -> 'a t
  val fail : T.t -> 'a t
end
