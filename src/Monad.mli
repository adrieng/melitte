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
  val (let*) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
  val (and*) : 'a M.t -> 'b M.t -> ('a * 'b) M.t
  val (let+) : 'a M.t -> ('a -> 'b) -> 'b M.t
  val (and+) : 'a M.t -> 'b M.t -> ('a * 'b) M.t
end
