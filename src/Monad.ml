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
