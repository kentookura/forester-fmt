open Pandoc
open Core

let empty_tree : Sem.tree =
  {
    title = None;
    taxon = None;
    authors = [];
    dates = [];
    addr = None;
    metas = [];
    tags = [];
    body = [];
    source_path = None;
  }

let node_of_block : Pandoc.block -> Sem.node Range.located = function
  | b ->
      let open Sem in
      let v =
        match b with
        | BulletList (_bss : block list list) -> Prim (`Ul, [])
        | CodeBlock ((_ : attr), (_ : string)) -> Prim (`Code, [])
        | Header ((_ : int), (_ : attr), (_ : inline list)) ->
            Text ""
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

let code_of_block : Pandoc.block -> Code.node Range.located = function
  | b ->
      let open Code in
      let v =
        match b with
        | BulletList (_bss : block list list) -> Prim (`Ul, [])
        | CodeBlock ((_ : attr), (_ : string)) -> Prim (`Code, [])
        | Header ((_ : int), (_ : attr), (_ : inline list)) ->
            Text ""
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

let code_of_pandoc : Pandoc.t -> Code.tree = function
  | _p -> { source_path = None; addr = ""; code = [] }

let tree_of_pandoc : Pandoc.t -> Sem.tree = function
  | p -> { empty_tree with body = List.map node_of_block (blocks p) }

let formatter_of_writer w =
  let out buf off len = Eio.Buf_write.string w buf ~off ~len in
  let flush () = () in
  Format.make_formatter out flush

let () =
  let module Write = Eio.Buf_write in
  let _tree =
    stdin |> Yojson.Basic.from_channel |> Pandoc.of_json |> tree_of_pandoc
    (* |> List.map node_of_block *)
  in
  ()
