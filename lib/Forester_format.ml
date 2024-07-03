module Pandoc_filter = Pandoc_filter
open Forester_frontend
open Lexing
open Pretty_expressive
module Core = Forester_core
open Core

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

let rec f (n : Syn.node) =
  let open Syn in
  match n with
  | Text s -> text s
  | Verbatim s -> text "<<<|" ^^ nl ^^ text s ^^ nl ^^ text "<<<"
  | Group (d, t) -> wrap (delim d) (format_code t)
  | Math (m, t) -> wrap (mode m) (format_code t)
  | Link { dest; title } -> (
      match title with
      | Some t ->
          wrap (delim Squares) (format_code t)
          ^^ wrap (delim Parens) (format_code dest)
      | None -> twice (wrap (delim Squares)) (format_code dest))
  | Transclude -> text "\\transclude"
  | Subtree (s, t) -> nest 2 (nl ^^ format_code t)
  | Query q -> text "s"
  | Embed_tex -> text "\\tex"
  | Lam (syms, t) -> text "s"
  | Var sym -> text "s"
  | Put (sym, t_1, t_2) -> text "\\put"
  | Default (sym, t_1, t_2) -> text "s"
  | Get sym -> text "s"
  | Xml_tag (_, _, _) -> text "s"
  | TeX_cs cs -> (
      match cs with Word s -> text s | Symbol c -> text (String.make 1 c))
  | Prim p -> (
      match p with
      | `Figure -> text "\\p"
      | `Em -> text "\\em"
      | `Figcaption -> text "\\figcaption"
      | `Strong -> text "\\strong"
      | `Ul -> text "\\ul"
      | `Li -> text "\\li"
      | `Blockquote -> text "\\blockquote"
      | `Code -> text "\\code"
      | `Ol -> text "\\ol"
      | `Pre -> text "\\pre"
      | `P -> text "\\p")
  | Object _ -> text "s"
  | Patch _ -> text "s"
  | Call (t, s) -> format_code t ^^ text s
  | Ref -> text "\\ref"
  | Title -> text "\\title"
  | Parent -> text "\\parent"
  | Taxon -> text "\\taxon"
  | Meta -> text "\\meta"
  | Author -> text "\\author"
  | Contributor -> text "\\contributor"
  | Tag -> text "\\tag"
  | Date -> text "\\date"
  | Number -> text "\\number"

and format_code (ns : Syn.tree) =
  List.map (fun n -> f Range.(n.value)) ns
  |> fold_doc (fun x y -> x <+> space <+> y)

let print_tree (t : Syn.tree) = format_code t |> pretty_print print_string
