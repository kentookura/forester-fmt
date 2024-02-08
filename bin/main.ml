open Forester_format

let () =
  let json = Yojson.Basic.from_file "pandoc.json" in

  let formatted =
    json |> Pandoc.of_json |> Pandoc_filter.code_of_pandoc |> format_code
  in
  Format.printf "\n%s\n" formatted
