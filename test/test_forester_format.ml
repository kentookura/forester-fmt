open Forester_format
open Forester_core
open Forester_frontend
module Tty = Asai.Tty.Make (Forester_core.Reporter.Message)

exception Foo

let roundtrip code =
  let string = P.pretty_format @@ format_code code in
  print_endline string;
  let new_code = Parse.parse_string string in
  match new_code with
  | Ok new_code ->
      (* print_endline @@ Code.show code; *)
      (* print_endline @@ Code.show new_code; *)
      assert (new_code = code)
  | Error _ -> ()

let test () =
  let code =
    Parse.parse_string
      {|
\title{asfd}

\p{
  asdf
  #{a=b}
}

\<asdf:lkj>[asfd]{lkjlk}{oewiruoeir}

\subtree[foasdf]{\p{}}

\fun[x][y]{Hello, \x and \y}

\def\my-fun[x][~y]{Hello, \x and \y{}!}

\def\my-query/author[x]{
 \open\query
 \rel{\edges}{\incoming}{\rel/authors}{\x}
}

\def\my-query/taxon[x]{
 \open\query
 \rel{\edges}{\incoming}{\rel/taxa}{\x}
}

\def\my-query/tag[x]{
 \open\query
 \rel{\edges}{\incoming}{\rel/tags}{\x}
}

% Allocate a fluid binding to hold the function that enters math mode.
\alloc\math-wrapper

% Ensure that a given (lazy) argument is evaluated inside a
% <mml:math> element.
\def\ensure-math[~body]{%
 \scope{%
  % Set a default value for \math-wrapper (no-op if already set).
  \put?\math-wrapper{\fun[x]{\<mml:math>{\x}}}%
  %
  % Apply the current function bound to \math-wrapper.
  \get\math-wrapper{%
   % Bind \math-wrapper to the identity function in the current scope so that
   % we do not get redundant <mml:math> nodes.
   \put\math-wrapper{\fun[x]{\x}}%
   %
   % Force the \body thunk to evaluate the body within the wrapper.
   \body{}%
  }%
 }%
}

\patch{\my-object}[self]{
  [method1]{
    \self/super#method1
    some further code
  }
}
|}
  in
  match code with Ok code -> roundtrip code | Error _ -> raise Foo

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Eio_main.run @@ fun _ ->
  Reporter.run ~emit:Tty.display ~fatal @@ fun () -> test ()
