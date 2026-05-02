{
  description = "Reflex Dom";

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nixcache.reflex-frp.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
  
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, haskellNix, nixpkgs, nix-filter, ... }@inputs:
    let
      system = "x86_64-linux";
      ghcVersion = "ghc914";
      overlays = [
        haskellNix.overlay
        (final: _prev: {
          # This overlay adds our project to pkgs
          myPackage =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc914";
              modules = [
              ];
              shell.tools = {
                cabal = {};
              };
              shell.buildInputs = with pkgs; [
                ghciwatch
                just
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.myPackage.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in {
      devShells.${system}.default = flake.devShells.default;
    };
}
