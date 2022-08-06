open Sexplib.Conv

type t = string [@@deriving sexp_of, eq, ord]

let pp fmt s = Format.fprintf fmt "%s" s

let dummy = "_"

let of_option = Option.value ~default:dummy
