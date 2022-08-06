let parse inp =
  let fname, ic =
    match inp with
    | `Stdin -> "*stdin*", stdin
    | `File fname -> fname, open_in fname
  in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  Sedlexing.set_filename lexbuf fname;
  let token = Sedlexing.with_tokenizer Lex.token lexbuf in
  let file = MenhirLib.Convert.Simplified.traditional2revised Parse.file in
  let raw = file token in
  close_in ic;
  raw

let process inp =
  try
    let raw = parse inp in
    Printf.printf "{- Raw source code -}\n";
    ExtPrint.to_out (Raw.PPrint.file raw);
    let syn = Elaborator.(M.run @@ check raw) in
    print_newline ();
    Printf.printf "{- Elaborated source code -}\n";
    ExtPrint.to_out (Syntax.PPrint.file syn)
  with Error.Error err ->
    Format.eprintf "%a@." Error.print err;
    exit 1

let () =
  let open Arg in
  let process_stdin = ref false in
  let inputs = ref [] in
  parse
    (align
       [
         "-stdin", Set process_stdin, " read standard input";
         "-encoding", Symbol (["utf8"; "ascii"],
                              fun s -> UnicodeSigil.encoding_of_string s
                                       |> Option.get
                                       |> UnicodeSigil.set_encoding),
         " set output encoding";
         "-type-in-type", Set Options.type_in_type,
         " accept type-in-type (inconsistent)";
       ]
    )
    (fun s -> inputs := `File s :: !inputs)
    (Printf.sprintf "Usage: %s [OPTIONS] file1.tt ... fileN.tt" Sys.argv.(0));
  if !process_stdin then inputs := !inputs @ [`Stdin];
  List.iter process !inputs
