{

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          };
        in
        {
          packages = {
            default = pkgs.haskellPackages.hookmark;
            pedantic = self.packages.${system}.default.overrideAttrs (oldAttrs: {
              configureFlags = oldAttrs.configureFlags ++ [ "-fpedantic" ];
            });
          };

          devShells.default =
            pkgs.haskellPackages.shellFor {
              packages = p: [ p.hookmark ];
              withHoogle = true;
              buildInputs = with pkgs.haskellPackages; [
                ormolu
              ];
            };

          apps.checkApply = {
            type = "app";
            program =
              let
                bin = pkgs.writeShellApplication {
                  name = "hookmark-check-apply";
                  runtimeInputs = with pkgs.haskellPackages; [
                    cabal-install
                    hlint
                    ormolu
                  ];
                  text = ''
                    	cabal-fmt -i hookmark.cabal
                    	./misc/hlintApply src test hookmark hookmark-web
                    	./misc/formatApply src test hookmark hookmark-web
                  '';
                };
              in
              "${bin}/bin/hookmark-check-apply";
          };
        })
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = (finalHaskellPackages: prevHaskellPackages: {
            hookmark =
              finalHaskellPackages.generateOptparseApplicativeCompletions [ "hookmark" "hookmark-web" ]
                ((finalHaskellPackages.callPackage ./cabal.nix { }).overrideAttrs (oldAttrs: {
                  checkInputs = with finalHaskellPackages; [
                    cabal-fmt
                    hlint
                    ormolu
                  ];
                  checkPhase = ''
                    ${oldAttrs.checkPhase}

                    ${finalHaskellPackages.cabal-install}/bin/cabal check
                    cabal-fmt hookmark.cabal | diff hookmark.cabal -
                    ./misc/hlintCheck src test hookmark hookmark-web
                    ./misc/formatCheck src test hookmark hookmark-web
                  '';
                }));
          });
        };
      };
    };
}
