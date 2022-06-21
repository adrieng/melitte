let ribbon = 1.0

let width = 80

let to_out ?(out = stdout) d =
  PPrint.ToChannel.pretty ribbon width out d

let to_string d =
  let b = Buffer.create 100 in
  PPrint.ToBuffer.pretty ribbon width b d;
  Buffer.contents b
