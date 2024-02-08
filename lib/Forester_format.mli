(* val format_code : Core.Syn.tree -> string *)
(* val load_file : string -> Core.Syn.t *)
module Pandoc_filter = Pandoc_filter

val format_code : Core.Syn.tree -> string
val load_file : string -> Core.Code.t
