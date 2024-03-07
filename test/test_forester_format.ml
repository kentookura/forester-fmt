open Forester_format
open Core.Syn

let () =
  let code = Text "asdf" in
  let tree =
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
      [ { Asai.Range.loc = None; value = code } ] )
  in

  let formatted = format_code tree in
  Format.printf "\n%s\n" formatted
