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

let format_node (node : Syn.node Range.located) =
  let open P in
  match node.value with
  | Syn.Text s -> text s
  | Syn.Group (_, _) -> text ""
  | Syn.Math (_, _) -> text ""
  | Syn.Link _ -> text ""
  | Syn.Transclude addr -> cmd "transclude" (text addr)
  | Syn.Query _ -> text ""
  | Syn.Embed_tex _ -> text ""
  | Syn.Block (_, _) -> text ""
  | Syn.Lam (_, _) -> text ""
  | Syn.Var _ -> text ""
  | Syn.Put (_, _, _) -> text ""
  | Syn.Default (_, _, _) -> text ""
  | Syn.Get _ -> text ""
  | Syn.If_tex (_, _) -> text ""
  | Syn.Xml_tag (_, _, _) -> text ""
  | Syn.Unresolved _ -> text ""
  | Syn.Prim (prim, _nodes) -> (
      match prim with
      | `Em -> cmd "em" (text "")
      | `Strong -> cmd "strong" (text "")
      | `Ul -> cmd "ul" (text "")
      | `Li -> cmd "li" (text "")
      | `Blockquote -> cmd "blockquote" (text "")
      | `Code -> cmd "code" (text "")
      | `Ol -> cmd "ol" (text "")
      | `Pre -> cmd "pre" (text "")
      | `P -> cmd "p" (text ""))
  | Syn.Object _ | Syn.Patch _ -> text ""
  | Syn.Call (_, _) -> text ""

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
