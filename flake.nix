{
  inputs = {
    make-shell.url = "github:ursi/nix-make-shell/1";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    purs-nix.url = "github:ursi/purs-nix";
    purs-nix.inputs.nixpkgs.follows = "nixpkgs";
    mk-node.url = "github:sephii/mk-node";
    mk-node.inputs.nixpkgs.follows = "nixpkgs";
    utils.url = "github:ursi/flake-utils/8";
  };

  outputs = { utils, mk-node, ... }@inputs:
    utils.apply-systems
      { inherit inputs; overlays = [ mk-node.overlay ]; }
      ({ make-shell, pkgs, purs-nix, system, ... }:
        let
          inherit (purs-nix) ps-pkgs purs;
          purs' = purs {
            dependencies =
              with ps-pkgs; [
                concur-core
                concur-react
                console
                effect
                prelude
                simple-json
                spec
                quickcheck
                spec-quickcheck
                stringutils
                parsing
              ];
            srcs = [ ./src ];
          };
          inherit (purs') command modules;
          nodeModules = pkgs.mkNodeModules { src = ./.; };
        in
        {
          defaultPackage = pkgs.stdenv.mkDerivation {
              name = "swq";
              src = ./.;
              dontBuild = true;
              buildInputs = with pkgs; [ 
                nodePackages.parcel-bundler
                pandoc
                (command {})
                aha
              ];
              installPhase = ''
                # set -xe 

                ln -s ${nodeModules}/lib/node_modules ./node_modules
                # export PATH="${nodeModules}/bin:$PATH"

                # cp -r $'{output}/ ./output
                purs-nix bundle
                # ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level SIMPLE_OPTIMIZATIONS --js index.js --js_output_file index.prod.js
                sed 's/script src="[a-zA-Z\.\/]*"/script src=".\/index.js"/g' ./static/index.html > index.html
                parcel build -d $out --public-url ./ index.html # --experimental-scope-hoisting 

                cp ./aufgabe/{anforderungen.md,doku.css} .
                pandoc anforderungen.md -s --toc --metadata pagetitle="Dokumentation" -c doku.css -o $out/docs.html
                cp doku.css $out
                cp ./aufgabe/architecture-overview-diagram.svg $out

                purs-nix test | aha -t Testergebnisse > $out/tests.html
              '';
            };

          devShell = make-shell {
            packages = with pkgs; [
              entr
              # nodejs
              (command { package.pursuit = {
                name = "kontaktsplitter";
                license.spdxId = "BSD";
                repo = "https://github.com/Matthi42/swq";
              }; })
              purs-nix.purescript
              purs-nix.purescript-language-server
              nodePackages.parcel-bundler
              nodePackages.npm
              nodePackages.purty
              # nodePackages.mocha
              nodeModules
              miniserve
              pandoc
              # nodePackages.rimraf
            ];

            aliases = {
              # docs = "npm run examples-prod && cp docs/logo.png dist && cp docs/Purescript-Concur-Performance.png dist && rimraf docs && mv dist docs";
            };
            setup = ''
              rm node_modules
              ln -fs ${nodeModules}/lib/node_modules ./node_modules
              export NODE_PATH="${nodeModules}/lib/node_modules" 
              # export PATH="${nodeModules}/bin:$PATH"
            '';
          };
        }
      );
}
