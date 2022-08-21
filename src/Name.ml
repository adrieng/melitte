(** Strings are represented as unique identifiers. *)

type t = int

let compare (x : t) y = Stdlib.compare x y

let equal x y = x = y

let fwd_table : (string, int) Hashtbl.t = Hashtbl.create 100
let bwd_table : (int, string) Hashtbl.t = Hashtbl.create 100
let free = ref 0

let of_string =
  fun s ->
  try Hashtbl.find fwd_table s
  with Not_found ->
    let id = !free in
    Hashtbl.add fwd_table s id;
    Hashtbl.add bwd_table id s;
    incr free;
    id

let to_string n = Hashtbl.find bwd_table n

let sexp_of_t n = Sexplib.Conv.sexp_of_string (to_string n)

let pp n = PPrint.utf8string (to_string n)

let dummy_prefix = "_"

let dummy = of_string dummy_prefix

let of_option = Option.value ~default:dummy

let internal s = of_string (dummy_prefix ^ s)
