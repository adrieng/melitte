module type Plain = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Runnable = sig
  include Plain
  val run : 'a t -> 'a
end

module Notation (M : Plain) = struct
  let return = M.return

  let ( let* ) = M.bind

  let ( and* ) x y =
    let* z = x in
    let* m = y in
    M.return (z, m)

  let ( let+ ) x f =
    let* y = x in
    M.return (f y)

  let ( and+ ) = ( and* )
end

module type TYPE = sig
  type t
end

module Reader (M : sig type t end) = struct
  type 'a t = M.t -> 'a
  let return x _ = x
  let bind x f s = f (x s) s
  let get s = s
  let run s x = x s
end

module State(T : TYPE) = struct
  type 'a t = T.t -> 'a * T.t

  let return x = fun s -> x, s

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun s -> let y, s = x s in f y s

  let run s x = let y, _ = x s in y

  let get s = s, s

  let set s _ = (), s
end

module Error(T : TYPE) = struct
  type 'a t = ('a, T.t) Result.t

  let return x = Ok x

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    match x with
    | Ok x -> f x
    | Error err -> Error err

  let fail err = Error err

  let run x = x
end

module ErrorT(T : TYPE)(M : Plain) = struct
  type 'a t = ('a, T.t) Result.t M.t

  let return x = M.return (Ok x)

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    M.bind x
      (function
       | Ok x -> f x
       | Error err -> M.return (Error err))

  let fail err = M.return @@ Error err

  let lift x = M.bind x (fun v -> M.return @@ Ok v)
end

