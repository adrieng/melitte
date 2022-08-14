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

let pass banner pp f input =
  let output = f input in
  if !Options.verbose
  then
    begin
      Printf.printf "{- %s -}\n" banner;
      ExtPrint.to_out (pp output);
      print_newline (); flush stdout;
    end;
  output

let pp_env fmt env =
  Format.fprintf fmt " in [@[%a@]]"
    (ExtPrint.to_fmt Semantics.PPrint.env) env

let on_check_pre env ~expected tm =
  if !Options.debug
  then
    Format.eprintf "@[<hv 2>@[%a@]@ %a? @[%a@]@[%a@]@]@."
      (ExtPrint.to_fmt Raw.PPrint.term) tm
      UnicodeSigil.pp UnicodeSigil.dlarrow
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) expected
      (Sigs.Formatter.pp_if !Options.verbose pp_env) env

let on_infer_pre env tm =
  if !Options.debug
  then
    Format.eprintf "@[<hv 2>@[%a@]@ %a?@[%a@]@]@."
      (ExtPrint.to_fmt Raw.PPrint.term) tm
      UnicodeSigil.pp UnicodeSigil.drarrow
      (Sigs.Formatter.pp_if !Options.verbose pp_env) env

let on_conversion_pre env ~expected ~actual _loc =
  if !Options.debug
  then
    Format.eprintf "@[<hv 2>@[%a@]@ <:? @[%a@]@]@."
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) actual
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) expected

let on_check_post env ~expected tm =
  if !Options.debug
  then
    Format.eprintf "@[<hv 2>@[%a@]@ %a%a @[%a@]@]@."
      (ExtPrint.to_fmt Raw.PPrint.term) tm
      UnicodeSigil.pp UnicodeSigil.dlarrow
      UnicodeSigil.pp UnicodeSigil.checkmark
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) expected

let on_infer_post env tm ~actual =
  if !Options.debug
  then
    Format.eprintf "@[<hv 2>@[%a@]@ %a%a @[%a@]@]@."
      (ExtPrint.to_fmt Raw.PPrint.term) tm
      UnicodeSigil.pp UnicodeSigil.drarrow
      UnicodeSigil.pp UnicodeSigil.checkmark
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) actual

let on_conversion_post env ~expected ~actual _loc =
  if !Options.debug
  then
    Format.eprintf "@[<hv 2>@[%a@]@ <:%a @[%a@]@]@."
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) actual
      UnicodeSigil.pp UnicodeSigil.checkmark
      (ExtPrint.to_fmt (fun v -> Semantics.PPrint.value v env)) expected

let process inp =
  try
    pass "Raw code" Raw.PPrint.file parse inp
    |> pass
         "Elaborated code"
         Core.PPrint.file
         Elaborator.(fun x -> run
                                ~on_check_pre
                                ~on_infer_pre
                                ~on_conversion_pre
                                ~on_check_post
                                ~on_infer_post
                                ~on_conversion_post
                              @@ check x)
    |> ignore
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
         "-debug", Set Options.debug,
         " enable debugging features";
         "-v", Set Options.verbose,
         " print actions during elaboration";
       ]
    )
    (fun s -> inputs := `File s :: !inputs)
    (Printf.sprintf "Usage: %s [OPTIONS] file1.tt ... fileN.tt" Sys.argv.(0));
  Printexc.record_backtrace !Options.debug;
  if !process_stdin then inputs := !inputs @ [`Stdin];
  List.iter process !inputs
