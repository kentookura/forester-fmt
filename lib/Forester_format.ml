module Pandoc_filter = Pandoc_filter
open Forester
open Core
open Lexing
open Pretty_expressive

let w = 80
let cf = Printer.default_cost_factory ~page_width:w ()

module P = Printer.Make ((val cf))
open P

let backslash c = text "\\" ^^ c
let braces c = text "{" ^^ c ^^ text "}"
let _textcmd label arg = backslash @@ text label ^^ braces (text arg)
let cmd t n = text "\\" ^^ text t ^^ text "{" ^^ n ^^ text "}"

let _delim d =
  let f (a, b) = (text a, text b) in
  match d with
  | Squares -> f ("[", "]")
  | Braces -> f ("{", "}")
  | Parens -> f ("(", ")")

let rec format_node (node : Syn.node Range.located) =
  let open P in
  let open Syn in
  match node.value with
  | Text s -> text s
  | Group (_, _) -> text "group"
  | Math (_, _) -> text "math"
  | Link _ -> text "link"
  | Transclude addr -> cmd "transclude" (text addr)
  | Query _ -> text "query"
  | Embed_tex _ -> text "embed"
  | Block (_, _) -> text "block"
  | Lam (_, _) -> text "lam"
  | Var _ -> text "var"
  | Put (_, _, _) -> text "put"
  | Default (_, _, _) -> text "default"
  | Get _ -> text "get"
  | If_tex (_, _) -> text "iftex"
  | Xml_tag (_, _, _) -> text "xml"
  | Unresolved _ -> text "unresolved"
  | Prim (p, n) -> format_prim (p, n)
  | Object _ -> text "obj"
  | Patch _ -> text "patch"
  | Call (_, _) -> text "call"

and format_prim (prim, nodes) =
  match prim with
  | `Em -> cmd "em" (hcat @@ List.map format_node nodes)
  | `Strong -> cmd "strong" (hcat @@ List.map format_node nodes)
  | `Ul -> cmd "ul" (hcat @@ List.map format_node nodes)
  | `Li -> cmd "li" (hcat @@ List.map format_node nodes)
  | `Blockquote -> cmd "blockquote" (hcat @@ List.map format_node nodes)
  | `Code -> cmd "code" (hcat @@ List.map format_node nodes)
  | `Ol -> cmd "ol" (hcat @@ List.map format_node nodes)
  | `Pre -> cmd "pre" (hcat @@ List.map format_node nodes)
  | `P -> cmd "p" (hcat @@ List.map format_node nodes)

let format_code ((_frontmatter, mainmatter) : Syn.tree) =
  let query q =
    let open Query in
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

  P.pretty_print
  @@ fold_doc (fun x y -> x <+> space <+> y) (List.map format_node mainmatter)

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
