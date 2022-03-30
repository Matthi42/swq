{
  description = "swq";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    (flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ ];
          pkgs = import nixpkgs { inherit system overlays; };
          hp = pkgs.haskellPackages; # pkgs.haskell.packages.ghc921;
          project = returnShellEnv:
            hp.developPackage {
              inherit returnShellEnv;
              name = "swq";
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; { };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (with hp; [
                    cabal-fmt
                    cabal-install
                    ghcid
                    haskell-language-server
                    fourmolu
                    pkgs.nixpkgs-fmt
                  ]);
            };
        in
        {
          defaultPackage = project false; 
          devShell = project true;

          checks = {
            pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                hlint.enable = true;
                nixpkgs-fmt.enable = true;
                fourmolu.enable = true;
              };
            };
          };
        }));
}
