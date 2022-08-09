type encoding =
  | ASCII
  | UTF8

let encoding_of_string = function
  | "ascii" -> Some ASCII
  | "utf8" -> Some UTF8
  | _ -> None

let encoding = ref UTF8

let set_encoding s = encoding := s

type t =
  {
    utf8 : string;
    ascii : string;
  }

let string s =
  match !encoding with
  | ASCII -> s.ascii
  | UTF8 -> s.utf8

let doc s =
  match !encoding with
  | ASCII -> PPrint.string s.ascii
  | UTF8 -> PPrint.utf8string s.utf8

let pp = ExtPrint.to_fmt doc

let make codepoints ascii_fallback =
  { utf8 = Array.map Uchar.of_int codepoints
           |> Sigs.Unicode.utf8_string_of_uchar_array;
    ascii = ascii_fallback; }

let lambda = make [| 955 |] "\\"

let forall = make [| 8704 |] "forall"

let srarrow = make [| 8594 |] "->"

let drarrow = make [| 8658 |] "=>"

let slarrow = make [| 8592 |] "<-"

let dlarrow = make [| 8656 |] "<="

let typ = make [| 120140 |] "Type"

let nat = make [| 8469 |] "Nat"

let tripleq = make [| 0x2261 |] "=="

let checkmark = make [| 0x2713 |] "ok"
