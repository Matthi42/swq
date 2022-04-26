{
  inputs = {
    make-shell.url = "github:ursi/nix-make-shell/1";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    purs-nix.url = "github:ursi/purs-nix";
    purs-nix.inputs.nixpkgs.follows = "nixpkgs";
    utils.url = "github:ursi/flake-utils/8";
  };

  outputs = { utils, ... }@inputs:
    utils.apply-systems { inherit inputs; }
      ({ make-shell, pkgs, purs-nix, ... }:
        let
          inherit (purs-nix) ps-pkgs purs;

          inherit
            (purs
              {
                dependencies =
                  with ps-pkgs;
                  [
                    concur-core
                    concur-react
                    console
                    effect
                    prelude
                  ];
                srcs = [ ./src ];
              })
            command
            modules;
        in
        {
          defaultPackage = modules.Main.app { name = "swq"; };

          devShell = make-shell {
            packages = with pkgs; [
              entr
              nodejs
              purs-nix.purescript
              purs-nix.purescript-language-server
              (command { })
              # miniserve
              # closurecompiler
              nodePackages.parcel-bundler
              # nodePackages.rimraf
            ];

            aliases = {
              # docs = "npm run examples-prod && cp docs/logo.png dist && cp docs/Purescript-Concur-Performance.png dist && rimraf docs && mv dist docs";
            };
          };

          packages =
            with modules.Main;
            {
              bundle = bundle { };
              output = output { };
            };
        }
      );
}
