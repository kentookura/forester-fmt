open Forester
open Pretty_expressive
open Core
open Lexing
open Query
module Terminal = Asai.Tty.Make (Reporter.Message)

let format_code (doc : Code.t) (w : int) =
  let cf = Printer.default_cost_factory ~page_width:w () in
  let module P = Printer.Make ((val cf)) in
  let open P in
  let bs c = text "\\" ^^ c in
  let braces c = text "{" ^^ c ^^ text "}" in
  let textcmd label arg = bs @@ text label ^^ braces (text arg) in
  let query q =
    let t =
      match q with
      | Author a | Tag a | Taxon a -> text "/author" ^^ text a
      | Meta (_s, a) -> text "/meta" ^^ text a
      | Or a -> text "/or" ^^ hcat @@ List.map (fun _str_t -> text "foo") a
      | And a -> text "/and" ^^ hcat @@ List.map (fun _str_t -> text "foo") a
      | Not _a -> text "/not" ^^ text "todo"
      | True -> text "\\true"
    in
    text "\\query/" ^^ t
  in
  let _queries = List.map query in

  let cmd t n = text "\\" ^^ text t ^^ text "{" ^^ n ^^ text "}" in
  let delim d =
    let f (a, b) = (text a, text b) in
    match d with
    | Squares -> f ("[", "]")
    | Braces -> f ("{", "}")
    | Parens -> f ("(", ")")
  in
  let rec pretty_node (node : Code.node) =
    match node with
    | Text s -> P.text s
    | Ident (l, n) -> bs @@ hcat (List.map text l) ^^ hcat (List.map text n)
    | Group (d, m) ->
        let f (a, b) m = hcat [ a; m; b ] in
        f (delim d) (pretty m)
    (* | Xml_tag (r, s, t) -> P.text "\\xml{}" *)
    | Xml_tag (ls, _, n) -> P.text "\\xml" ^^ text ls ^^ pretty n ^^ nl
    | Transclude addr -> textcmd "transclude" addr
    | Embed_tex _ -> P.text "todo"
    | Let _ -> P.text "\\let"
    | Prim (t, n) -> (
        match t with
        | `P -> cmd "p" @@ pretty n
        | `Ol -> cmd "ol" @@ pretty n
        | `Ul -> cmd "ul" @@ pretty n
        | `Li -> cmd "li" @@ pretty n
        | `Em -> cmd "em" @@ pretty n
        | `Strong -> cmd "strong" (pretty n)
        | `Code -> cmd "code" (pretty n)
        | `Blockquote -> cmd "blockquote" (pretty n)
        | `Pre -> cmd "pre" (pretty n))
    (* | Object { self = s; methods = ms } -> obj s ms *)
    | Object _ -> text "\\object"
    | Patch _ -> P.text "patch"
    | Call _ -> P.text "call"
    | Query _ -> P.text "\\query"
    | Def (_, _, n) -> nl ^^ text "\\def" ^^ pretty n ^^ nl
    | Alloc _ -> P.text "\\alloc"
    | Title a -> text "\\title{" ^^ pretty a ^^ text "}"
    | Meta (_, _) -> P.text "\\meta" ^^ nl
    | Author a -> textcmd "author" a ^^ nl
    | Tag t -> textcmd "tag" t ^^ nl
    | TeX_package _ -> P.text "\\texpackage"
    | Date d -> textcmd "date" d ^^ nl
    | Namespace _ -> P.text "\\namespace"
    | Math (mode, n) -> (
        match mode with
        | Inline -> P.text "#{ " ^^ pretty n ^^ text "}"
        | Display -> P.text "##{" ^^ pretty n ^^ text "}")
    | Open _ -> P.text "todo"
    | Scope ns -> nl ^^ text "\\scope{" ^^ pretty ns ^^ text "}" ^^ nl
    | Put (l, t) ->
        P.text "\\put"
        ^^ (hcat @@ List.map (fun c -> text "\\" ^^ text c) l)
        ^^ pretty t
    | Default _ -> P.text "todo"
    | Get _ -> P.text "todo"
    | If_tex _ -> P.text "todo"
    | Import (vis, addr) -> (
        match vis with
        | Private -> textcmd "import" addr ^^ nl
        | Public -> textcmd "export" addr ^^ nl)
    | Taxon t -> textcmd "taxon" t ^^ nl
    | Block _ -> P.text "todo"
  and pretty d =
    match d with
    | [] -> P.text ""
    (* TODO: check which constructs are fine to reorder. Frontmatter only?  *)
    | node :: nodes -> pretty_node node.value <+> pretty nodes
  in
  P.pretty_print @@ pretty doc

let load_file filepath =
  Reporter.tracef "when parsing file `%s`" filepath @@ fun () ->
  let lexbuf = Lexing.from_channel (open_in filepath) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  try Grammar.main Lexer.token lexbuf with
  | Grammar.Error ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "failed to parse `%s`"
        (Lexing.lexeme lexbuf)
  | Lexer.SyntaxError token ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "unrecognized token `%s`"
      @@ String.escaped token

let () =
  let display d = Terminal.display d in
  match Sys.argv.(1) with
  | filepath ->
      Reporter.run ~emit:display ~fatal:(fun d ->
          display d;
          exit 1)
      @@ fun () ->
      let code = load_file filepath in
      Format.printf "\n%s" (format_code code 80)
