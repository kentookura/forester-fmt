{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, opam-repository }@inputs:
    let package = "forester_format";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocaml-base-compiler = "5.2.0";
          ocaml-lsp-server = "*";
          pandoc = "*";
          forester = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // { };
        scope = on.buildOpamProject' {
          repos = [ "${opam-repository}" ];
          pinDepends = true;
        } ./. query;
        overlay = final: prev: {
          ${package} =
            prev.${package}.overrideAttrs (_: { doNixSupport = false; });
        };
        scope' = scope.overrideScope' overlay;
        main = scope'.${package};
        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      in {
        legacyPackages = scope';

        packages.default = main;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ main ];
          buildInputs = with pkgs;
            devPackages ++ [ pandoc libev pkg-config openssl ];
        };
      });
}
