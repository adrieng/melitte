open Sexplib.Conv

type width = int

module type DB = sig
  type t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val equal : t -> t -> bool
  val to_int : t -> int
  val fresh : free:width -> t
end

module Ix = struct
  type t = int [@@deriving eq, sexp_of]
  let to_int i = i
  let fresh ~free = ignore free; 0
  let shift n = n + 1
end

module Lv = struct
  type t = int [@@deriving eq, sexp_of]
  let to_int i = i
  let fresh ~free = free
end

let lv_of_ix ~free l =
  if l >= free then invalid_arg "lv_of_ix";
  free - l - 1

let ix_of_lv ~free n =
  if n >= free then invalid_arg "ix_of_lv";
  free - n - 1

module Env = struct
  type 'a t = { w : int; c : 'a list; } [@@deriving sexp_of]

  let width { w; _ } = w

  let well_scoped_lv { w; _ } lv =
    assert (lv >= 0);
    lv < w

  let empty = { w = 0; c = []; }

  let extend v env = { w = env.w + 1; c = v :: env.c; }

  let lookup ix env =
    if ix >= env.w then raise Not_found;
    List.nth env.c ix

  let find p { c; w; } =
    let rec loop ix = function
      | [] -> assert (ix = w); raise Not_found
      | x :: c -> if p x then ix, x else loop (ix + 1) c
    in
    loop 0 c

  let fold f { c; _ } acc =
    List.fold_right f c acc

  let rec fold_cons f env acc =
    match env.c with
    | [] ->
       acc
    | entry :: c ->
       let env' = { c; w = env.w - 1; } in
       f entry env @@ fold_cons f env' acc

  let map f { c; w; } =
    { c = List.map f c; w; }

  let to_seq { c; _ } =
    List.to_seq c
end
