{
  description = "A flake demonstrating how to build OCaml projects with Dune";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in flake-utils.lib.eachDefaultSystem (system: {
      devShells = {
        default = pkgs.mkShell {
          packages = with pkgs.ocaml-ng.ocamlPackages_5_0;
            with pkgs; [
              dune_3
              opam
              ocaml
            ];
          shellHook = "eval $(opam env --switch=5.1.0)";
        };
      };
    });
}
