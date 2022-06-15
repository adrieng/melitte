let parse inp =
  let fname, ic =
    match inp with
    | `Stdin -> "*stdin*", stdin
    | `File fname -> fname, open_in fname
  in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname; };
  let raw = Parse.file Lex.token lexbuf in
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
         "-dbg-lexer", Set Lex.debug, " print lexeme stream";
       ]
    )
    (fun s -> inputs := `File s :: !inputs)
    (Printf.sprintf "Usage: %s [OPTIONS] file1.tt ... fileN.tt" Sys.argv.(0));
  if !process_stdin then inputs := !inputs @ [`Stdin];
  List.iter process !inputs
