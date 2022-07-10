{
  description = "Based flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          cs =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc902";
              shell.tools = {
                cabal = {};
                hpack = {};
                haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];

      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

      flake = pkgs.cs.flake {};

    in flake // {
      defaultPackage = flake.packages."cs:exe:server";
    });
}
