{
  description = "Advent of Code 2025 - Haskell Solutions";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "adventofcode2025";

        haskellDeps = ps: with ps; [
          base
          file-embed
          text
          bytestring
        ];

        haskellTools = with haskellPackages; [
          ghc
          cabal-install
          haskell-language-server
          hlint
          ghcid
          hpack
          hoogle
        ];

        # Build the Haskell package
        aoc2025 = haskellPackages.developPackage {
          root = ./.;
          name = packageName;
          returnShellEnv = false;
        };

      in {
        packages.default = aoc2025;

        apps.default = {
          type = "app";
          program = "${aoc2025}/bin/aoc2025";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = haskellTools ++ [ pkgs.zlib ];

          inputsFrom = [ aoc2025.env ];
        };
      }
    );
}
