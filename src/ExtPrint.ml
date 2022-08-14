let ribbon = 1.0

let width = 80

let int i = PPrint.string @@ string_of_int i

let to_out ?(out = stdout) d =
  PPrint.ToChannel.pretty ribbon width out d

let to_string d =
  let b = Buffer.create 100 in
  PPrint.ToBuffer.pretty ribbon width b d;
  Buffer.contents b

let pp fmt doc = PPrint.ToFormatter.pretty ribbon width fmt doc

let to_fmt to_doc fmt x = pp fmt (to_doc x)

