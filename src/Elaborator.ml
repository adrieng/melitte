module S = Syntax

module Env = struct
  type t = { len : int;
             var : (string * Syntax.t) list; }

  let initial = { len = 0; var = []; }
end

module M = struct
  type 'a t = Env.t -> 'a * Env.t

  let return x = fun env -> x, env

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun env -> let y, env = x env in f y env

  let run x = let y, _ = x Env.initial in y
end

module Infix = Monad.Notation (M)

include M

let check : Raw.t -> Syntax.t t =
  fun _r -> return []
