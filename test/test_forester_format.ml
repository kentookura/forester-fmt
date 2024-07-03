open Forester_format
open Forester_core
open Forester_frontend
module Tty = Asai.Tty.Make (Forester_core.Reporter.Message)

exception Foo

let tree =
  let code = Parse.parse_string {|
\title{asfd}
\p{
  asdf
}
|} in
  match code with
  | Ok code -> Code.{ source_path = None; addr = Some "foo"; code }
  | Error _ -> raise Foo

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  (* Eio_main.run @@ fun _ -> *)
  Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let syn = Expand.expand_tree Expand.UnitMap.empty tree |> snd in
  print_tree syn
