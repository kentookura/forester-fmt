open Pandoc
open Core

let rec syn_of_inline : Pandoc.inline -> Syn.node Range.located = function
  | i ->
      let v =
        match i with
        | Code (_, (s : string)) ->
            Syn.Prim (`Code, [ { value = Syn.Text s; loc = None } ])
        | Emph (is : Pandoc.inline list) ->
            Syn.Prim (`Em, List.map syn_of_inline is)
        | Strong (is : Pandoc.inline list) ->
            Syn.Prim (`Strong, List.map syn_of_inline is)
        | Link ((_ : Pandoc.attr), (_ : Pandoc.inline list), (_ : Pandoc.target))
          ->
            Syn.Link { dest = []; title = None }
        | Image (_, (_ : Pandoc.inline list), (_ : Pandoc.target)) ->
            Syn.Text ""
        | Quoted ((_ : Pandoc.quote_type), (_ : Pandoc.inline list)) ->
            Syn.Text "quoted"
        | RawInline ((_ : string), (_ : string)) -> Syn.Text "rawinline"
        | Space -> Syn.Text " "
        | SmallCaps _ -> Syn.Text "caps"
        | Str s -> Syn.Text s
        | Underline (_ : Pandoc.inline list) -> Syn.Text "underline"
        | Strikeout (_ : Pandoc.inline list) -> Syn.Text ""
        | Superscript (_ : Pandoc.inline list) -> Syn.Text ""
        | Subscript (_ : Pandoc.inline list) -> Syn.Text ""
        | Cite ((_ : Pandoc.citation list), (_ : Pandoc.inline list)) ->
            Syn.Text ""
        | SoftBreak -> Syn.Text ""
        | LineBreak -> Syn.Text "\n"
        | Math (mode, s) ->
            let m =
              match mode with
              | DisplayMath -> Core.Display
              | InlineMath -> Core.Inline
            in
            Syn.Math (m, [ { value = Syn.Text s; loc = None } ])
        | Note (_ : Pandoc.block list) -> Syn.Text ""
        | Span ((_ : Pandoc.attr), (_ : Pandoc.inline list)) -> Syn.Text ""
        | UnhandledInline _ -> Syn.Text ""
      in
      { value = v; loc = None }

and syn_of_block : Pandoc.block -> Syn.node Range.located = function
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
        | Para (is : inline list) -> Prim (`P, List.map syn_of_inline is)
        | Plain (is : inline list) -> Prim (`P, List.map syn_of_inline is)
        | RawBlock ((_ : format), (_ : string)) -> Prim (`Pre, [])
        | Div ((_ : attr), (bs : block list)) ->
            Prim (`P, List.map syn_of_block bs)
        | LineBlock (_iss : inline list list) ->
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
