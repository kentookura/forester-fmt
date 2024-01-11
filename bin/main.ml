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
    | Ident (_, _) -> text "\\todo{todo}"
    | Group _ -> P.text "todo"
    (* | Xml_tag (r, s, t) -> P.text "\\xml{}" *)
    | Xml_tag _ -> P.text "\\xml{}"
    | Transclude addr -> textcmd "transclude" addr
    | Embed_tex _ -> P.text "todo"
    | Let _ -> P.text "todo"
    | Prim (t, n) -> (
        match t with
        | `P -> text "\\p{" ^^ pretty n ^^ text "}"
        | `Ol -> text "\\ol{" ^^ pretty n ^^ text "}"
        | `Ul -> text "\\ul{" ^^ pretty n ^^ text "}"
        | `Li -> text "\\li{" ^^ pretty n ^^ text "}"
        | `Em -> text "\\em{" ^^ pretty n ^^ text "}"
        | `Strong -> text "\\strong{" ^^ pretty n ^^ text "}"
        | `Code -> text "\\code{" ^^ pretty n ^^ text "}"
        | `Blockquote -> text "\\blockquote{" ^^ pretty n ^^ text "}"
        | `Pre -> text "\\pre{" ^^ pretty n ^^ text "}")
    | Object _ -> P.text "todo"
    | Patch _ -> P.text "todo"
    | Call _ -> P.text "todo"
    | Query _ -> P.text "todo"
    | Def _ -> P.text "todo"
    | Alloc _ -> P.text "todo"
    | Title a -> text "\\author{" ^^ pretty a ^^ text "}"
    | Meta _ -> P.text "todo"
    | Author a -> textcmd "author" a
    | Tag t -> textcmd "tag" t
    | TeX_package _ -> P.text "todo"
    | Date d -> textcmd "date" d
    | Namespace _ -> P.text "todo"
    | Math (mode, _) -> (
        match mode with
        | Inline -> P.text "#{todo}"
        | Display -> P.text "##{todo}")
    | Open _ -> P.text "todo"
    | Scope _ -> P.text "todo"
    | Put _ -> P.text "todo"
    | Default _ -> P.text "todo"
    | Get _ -> P.text "todo"
    | If_tex _ -> P.text "todo"
    | Import (_, addr) -> textcmd "import" addr
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
