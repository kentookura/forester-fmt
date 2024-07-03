(* open Forester_format *)

(*
let load_file filepath =
  Reporter.tracef "when parsing file `%s`" filepath @@ fun () ->
  let lexbuf = Lexing.from_channel (open_in filepath) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  try Grammar.main Lexer.token lexbuf with
  | Grammar.Error ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "failed to parse `%s`"
        (Lexing.lexeme lexbuf)
  | (_ : exn) -> []
(* | Lexer.SyntaxError token -> *)
(*     let loc = Asai.Range.of_lexbuf lexbuf in *)
(*     Reporter.fatalf ~loc Parse_error "unrecognized token `%s`" *)
(*     @@ String.escaped token *)
*)

let () = ()
(*
  let json = Yojson.Basic.from_file "pandoc.json" in

  let formatted =
    json |> Pandoc.of_json |> Pandoc_filter.code_of_pandoc |> format_code
  in
  Format.printf "\n%s\n" formatted
*)
