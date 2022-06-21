let encoding : [ `ASCII | `UTF8 ] ref = ref `UTF8

type t =
  {
    utf8 : string;
    ascii : string;
  }

let string s =
  match !encoding with
  | `ASCII -> s.ascii
  | `UTF8 -> s.utf8

let make codepoints ascii_fallback =
  { utf8 = Array.map Uchar.of_int codepoints
           |> Sigs.Unicode.utf8_string_of_uchar_array;
    ascii = ascii_fallback; }

let lambda = make [| 955 |] "\\"

let forall = make [| 8704 |] "forall"

let sarrow = make [| 8594 |] "->"

let darrow = make [| 8658 |] "=>"

let typ = make [| 120140 |] "Type"

let nat = make [| 8469 |] "Nat"
