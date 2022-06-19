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
    Format.printf "%a@." Raw.pp raw
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
       ]
    )
    (fun s -> inputs := `File s :: !inputs)
    (Printf.sprintf "Usage: %s [OPTIONS] file1.tt ... fileN.tt" Sys.argv.(0));
  if !process_stdin then inputs := !inputs @ [`Stdin];
  List.iter process !inputs
