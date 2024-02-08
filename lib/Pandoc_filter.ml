open Pandoc
open Core

let syn_of_inline : Pandoc.inline -> Syn.node Range.located = function
  | i ->
      let v =
        match i with
        | Code (_, _)
        | Emph _
        | Image (_, _, _)
        | Link (_, _, _)
        | Quoted (_, _)
        | RawInline (_, _)
        | Space | SmallCaps _ | Str _ | Underline _ | Strong _ | Strikeout _
        | Superscript _ | Subscript _
        | Cite (_, _)
        | SoftBreak | LineBreak
        | Math (_, _)
        | Note _
        | Span (_, _)
        | UnhandledInline _ ->
            Syn.Text ""
      in
      { value = v; loc = None }

let rec syn_of_block : Pandoc.block -> Syn.node Range.located = function
  | b ->
      let open Syn in
      let v =
        match b with
        | BulletList (bss : block list list) ->
            Prim (`Ul, List.map list_item bss)
        | CodeBlock ((_ : attr), (s : string)) ->
            Prim (`Code, [ { value = Text s; loc = None } ])
        | Header ((_level : int), (_ : attr), (is : inline list)) ->
            Prim (`Strong, List.map syn_of_inline is)
            (*  TODO: Maybe use subtrees here? Let's wait until the next version of forester*)
        | OrderedList ((_ : list_attributes), (_ : block list list)) ->
            Prim (`Ol, [])
        | Para (_ : inline list) -> Prim (`P, [])
        | Plain (_ : inline list) -> Prim (`P, [])
        | RawBlock ((_ : format), (_ : string)) -> Prim (`Pre, [])
        | Div ((_ : attr), (_ : block list)) -> Prim (`P, [])
        | LineBlock (_ : inline list list) ->
            Prim (`Pre, []) (*  TODO: Is this correct?*)
        | BlockQuote (_ : block list) -> Prim (`Blockquote, [])
        | DefinitionList (_ : (inline list * block list list) list) -> Text ""
        | HorizontalRule -> Text ""
        | Table (_, _, _, _, _, _) -> Text ""
        | Figure
            ((_ : Pandoc.attr), (_ : Pandoc.caption), (_ : Pandoc.block list))
          ->
            Text ""
        | UnhandledBlock (_ : Yojson.Basic.t) -> Text ""
      in
      { value = v; loc = None }

and list_item : block list -> Syn.node Range.located = function
  | bs -> { value = Prim (`Li, List.map syn_of_block bs); loc = None }

let code_of_pandoc : Pandoc.t -> Syn.tree = function
  | t ->
      let body = t |> blocks |> List.map syn_of_block in
      ( {
          title = None;
          addr = "";
          taxon = None;
          authors = [];
          tags = [];
          dates = [];
          metas = [];
          tex_packages = [];
          source_path = None;
        },
        body )
