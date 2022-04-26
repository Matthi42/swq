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
              })
            command;
        in
        {
          devShell = make-shell {
            packages = with pkgs; [
              entr
              nodejs
              purs-nix.purescript
              purs-nix.purescript-language-server
              (command { })
              miniserve
            ];

            aliases.watch = "find src | entr -s 'echo bundling; purs-nix bundle'";
          };
        }
      );
}
