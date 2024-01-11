open Pretty_expressive
open Forester
open Core
open Lexing
module Terminal = Asai.Tty.Make (Reporter.Message)

let format_code (doc : Code.t) (w : int) =
  let cf = Printer.default_cost_factory ~page_width:w () in
  let module P = Printer.Make ((val cf)) in
  let open P in
  let bs c = "\\" ^ c in
  let braces c = "{" ^ c ^ "}" in
  let textcmd label arg = P.text @@ bs @@ label ^ braces arg in
  let rec pretty_node (node : Code.node Range.located) =
    match node.value with
    | Text s -> P.text s
    | Ident (l, _) -> P.text @@ String.concat "" @@ List.map braces l
    | Group _ -> P.text "todo"
    | Xml_tag _ -> P.text "todo"
    | Transclude _ -> P.text "todo"
    | Embed_tex _ -> P.text "todo"
    | Let _ -> P.text "todo"
    | Prim _ -> P.text "todo"
    | Object _ -> P.text "todo"
    | Patch _ -> P.text "todo"
    | Call _ -> P.text "todo"
    | Query _ -> P.text "todo"
    | Def _ -> P.text "todo"
    | Alloc _ -> P.text "todo"
    | Title _ -> P.text "todo"
    | Meta _ -> P.text "todo"
    | Author a -> textcmd "author" a
    | Tag t -> textcmd "tag" t
    | TeX_package _ -> P.text "todo"
    | Date _ -> P.text "todo"
    | Namespace _ -> P.text "todo"
    | Math _ -> P.text "todo"
    | Open _ -> P.text "todo"
    | Scope _ -> P.text "todo"
    | Put _ -> P.text "todo"
    | Default _ -> P.text "todo"
    | Get _ -> P.text "todo"
    | If_tex _ -> P.text "todo"
    | Import _ -> P.text "todo"
    | Taxon t -> textcmd "taxon" t
    | Block _ -> P.text "todo"
  and pretty d =
    match d with
    | [] -> P.text ""
    | node :: nodes -> pretty_node node <+> pretty nodes
  in
  P.pretty_print @@ pretty doc

let load_file filepath =
  Reporter.tracef "when parsing file `%s`" filepath @@ fun () ->
  let lexbuf = Lexing.from_channel (open_in filepath) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  let code =
    try Grammar.main Lexer.token lexbuf with
    | Grammar.Error ->
        let loc = Asai.Range.of_lexbuf lexbuf in
        Reporter.fatalf ~loc Parse_error "failed to parse `%s`"
          (Lexing.lexeme lexbuf)
    | Lexer.SyntaxError token ->
        let loc = Asai.Range.of_lexbuf lexbuf in
        Reporter.fatalf ~loc Parse_error "unrecognized token `%s`"
        @@ String.escaped token
  in
  Format.printf "%s" (format_code code 80)

let () =
  let display d = Terminal.display d in
  match Sys.argv.(1) with
  | filepath ->
      Reporter.run ~emit:display ~fatal:(fun d ->
          display d;
          exit 1)
      @@ fun () -> load_file filepath
