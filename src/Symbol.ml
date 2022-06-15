module type S = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
  val fresh : ?name:string -> unit -> t
end

module Make() = struct
  type t =
    {
      name : string;
      id : int;
      first : bool;
    }

  let compare x y =
    Stdlib.compare x.id y.id

  let equal x y =
    x.id = y.id

  let hash x =
    Hashtbl.hash x.id

  let to_string x =
    x.name ^ if x.first then "" else "_" ^ string_of_int x.id

  let fresh =
    let module H = Hashtbl.Make(Sigs.String) in
    let r = ref 0 in
    let ht = H.create 100 in
    fun ?(name = "") () ->
    let first = name <> "" && not (H.mem ht name) in
    if first then H.add ht name ();
    let id = !r in incr r; { id; name; first; }
end
