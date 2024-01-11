open Pretty_expressive

(* Sets the page width limit to 80 *)
let cf = Printer.default_cost_factory ~page_width:80 ()

module P = Printer.Make ((val cf))

let print_doc (w : int) =
  let cf = Printer.default_cost_factory ~page_width:w () in
  let module P = Printer.Make ((val cf)) in
  let open P in
  let d =
    text "while (true) {"
    ^^ nest 4
         (nl ^^ text "f();" ^^ nl ^^ text "if (done())"
         ^^
         let exit_d = text "exit();" in
         space ^^ exit_d <|> nest 4 (nl ^^ exit_d))
    ^^ nl ^^ text "}"
  in
  pretty_print d

let () =
  let doc = print_doc 80 in
  Format.printf "\n%s\n" doc
