module Pandoc_filter = Pandoc_filter
open Pretty_expressive
open Forester_core

let w = 80
let cf = Printer.default_cost_factory ~page_width:w ()

module P = Printer.Make ((val cf))
open P

let backslash c = text "\\" ^^ c
let braces c = text "{" ^^ c ^^ text "}"
let _textcmd label arg = backslash @@ text label ^^ braces (text arg)
let cmd t n = text "\\" ^^ text t ^^ text "{" ^^ n ^^ text "}"
let mapt f (a, b) = (f a, f b)

let delim d =
  let t =
    match d with
    | Squares -> ("[", "]")
    | Braces -> ("{", "}")
    | Parens -> ("(", ")")
  in
  mapt text t

let mode m =
  let t = match m with Display -> ("##{", "}") | Inline -> ("#{", "}") in
  mapt text t

let wrap t n = fst t ^^ n ^^ snd t
let braces = wrap (delim Braces)
let parens = wrap (delim Parens)
let squares = wrap (delim Squares)
let twice f x = f (f x)

exception Todo

let format_symbol sym = text @@ Format.asprintf "%a" Symbol.pp sym

let pp_trie_path ppf segments =
  let pp_sep ppf () = Format.fprintf ppf "/" in
  Format.fprintf ppf "%a"
    Format.(pp_print_list ~pp_sep pp_print_string)
    segments

let format_trie_path p = text @@ Format.asprintf "%a" pp_trie_path p

let rec f (n : Code.node) =
  match n with
  | Code.Text s -> text s
  | Code.Verbatim s -> text s
  | Code.Group (d, nodes) ->
      let d =
        match d with Braces -> braces | Squares -> squares | Parens -> parens
      in
      d @@ format_code nodes
  | Code.Math (m, nodes) -> wrap (mode m) @@ format_code nodes
  | Code.Ident (path, methods) ->
      let methods =
        List.map (fun str -> text "#" ^^ text str) methods |> fold_doc ( ^^ )
      in
      text "\\" ^^ format_trie_path path ^^ methods
  | Code.Xml_tag (title, attrs, nodes) ->
      let title =
        match fst title with
        | None -> text @@ snd title
        | Some t -> text t ^^ text ":" ^^ text @@ snd title
      in
      text "\\<" ^^ title ^^ text ">"
      ^^ fold_doc ( <+> ) (List.map format_attr attrs)
      ^^ braces @@ format_code nodes
  | Code.Subtree (addr, nodes) ->
      let addr =
        match addr with None -> empty | Some addr -> squares @@ text addr
      in
      text "\\subtree" ^^ addr ^^ braces @@ format_code nodes
  | Code.Let (path, bindings, nodes) ->
      text "\\let" ^^ format_trie_path path
      ^^ fold_doc ( <+> ) (List.map format_binding bindings)
      ^^ braces @@ format_code nodes
  | Code.Open path -> text "\\open\\" ^^ format_trie_path path
  | Code.Scope nodes -> text "\\scope" ^^ braces @@ format_code nodes
  | Code.Put (path, nodes) ->
      text "\\put\\" ^^ format_trie_path path ^^ braces @@ format_code nodes
  | Code.Default (path, nodes) ->
      text "\\put?\\" ^^ format_trie_path path ^^ braces @@ format_code nodes
  | Code.Get path -> format_trie_path path
  | Code.Fun (bindinglist, nodes) ->
      text "\\fun"
      ^^ fold_doc ( <+> ) (List.map format_binding bindinglist)
      ^^ braces @@ format_code nodes
  | Code.Object { self; methods } ->
      let self =
        match self with Some path -> format_trie_path path | None -> empty
      in
      text "\\object" ^^ (squares @@ self)
      ^^ fold_doc ( <+> ) (List.map format_method methods)
  | Code.Patch patch -> (
      match patch with
      | { obj; self; methods } ->
          let self =
            match self with Some self -> format_trie_path self | None -> empty
          in
          text "\\patch"
          ^^ (braces @@ format_code obj)
          ^^ squares self ^^ braces
          @@ fold_doc ( <+> ) (List.map format_method methods))
  | Code.Call (obj, method_name) ->
      text "\\call" ^^ format_code obj ^^ text method_name
  | Code.Import (vis, str) ->
      let ident =
        match vis with Private -> text "\\import" | Public -> text "\\export"
      in
      ident ^^ braces @@ text str
  | Code.Def (path, bindinglist, nodes) ->
      text "\\def\\" ^^ format_trie_path path
      ^^ fold_doc ( <+> ) (List.map format_binding bindinglist)
      ^^ braces @@ format_code nodes
  | Code.Decl_xmlns (prefix, xmlns) ->
      text "\\xmlns:" ^^ text prefix ^^ braces @@ text xmlns
  | Code.Alloc path -> text "\\alloc\\" ^^ format_trie_path path
  | Code.Namespace (path, code) ->
      text "\\namespace" ^^ format_trie_path path ^^ format_code code

and format_method ((name, nodes) : string * Code.t) =
  (squares @@ text name) ^^ braces @@ format_code nodes

and format_binding ((strategy, path) : Forester_core__Trie.path Base.binding) =
  let strat = match strategy with Lazy -> text "~" | Strict -> empty in
  squares @@ strat ^^ format_trie_path path

and format_attr (((prefix, uname), v) : (string option * string) * Code.t) =
  let prefix =
    match prefix with None -> empty | Some str -> squares @@ text str
  in
  prefix ^^ (squares @@ text uname) ^^ braces @@ format_code v

and format_code (ns : Code.t) =
  List.map (fun n -> f Range.(n.value)) ns |> fold_doc (fun x y -> x <+> y)

let print_tree (t : Code.t) = format_code t |> pretty_print print_string
