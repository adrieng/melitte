open Sexplib.Conv

type t =
  | Fin of int
  | Inf
  [@@deriving sexp_of]

let fin l =
  if l < 0 then invalid_arg "finite";
  Fin l

let inf = Inf

let max l1 l2 =
  match l1, l2 with
  | _, Inf | Inf, _ -> Inf
  | Fin i, Fin j -> Fin (max i j)

let ( = ) (l1 : t) l2 = l1 = l2

let ( <= ) l1 l2 =
  match l1, l2 with
  | _, Inf -> true
  | Inf, _ -> false
  | Fin i, Fin j -> i <= j

let ( < ) l1 l2 =
  match l1, l2 with
  | Fin _, Inf -> true
  | Inf, _ -> false
  | Fin i, Fin j -> i < j

module PPrint = struct
  let level = function
    | Fin i -> PPrint.string @@ string_of_int i
    | Inf -> UnicodeSigil.(doc omega)
end
