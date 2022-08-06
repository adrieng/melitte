open Sexplib.Conv

type t = string [@@deriving sexp_of, eq, ord]

let dummy = "_"

let pp fmt s = Format.fprintf fmt "%s" s

(* module Env = struct *)
(*   module E = DeBruijn.Env *)
(*   type name = t *)
(*   type 'a t = (name * 'a) E.t *)
(*   let width = E.width *)
(*   let empty = E.empty *)
(*   let extend x v env = E.extend (x, v) env *)
(*   let lookup x env = *)
(*     let ix, (_, v) = E.find (fun (y, _) -> x = y) env in *)
(*     ix, v *)
(*   let map f = E.map (fun (x, v) -> x, f x v) *)
(*   let fold f = E.fold (fun (x, v) acc -> f x v acc) *)
(* end *)
