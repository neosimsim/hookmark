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
          packages.default =
            pkgs.haskellPackages.hookmark;

          devShells.default =
            pkgs.haskellPackages.shellFor {
              packages = p: [ p.hookmark ];
              withHoogle = true;
              buildInputs = with pkgs.haskellPackages; [
                ormolu
              ];
            };

        })
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = (finalHaskellPackages: prevHaskellPackages: {
            hookmark =
              (finalHaskellPackages.callPackage ./cabal.nix { }).overrideAttrs (oldAttr: {
                checkInputs = with finalHaskellPackages; [
                  cabal-fmt
                  hlint
                  ormolu
                ];
                preCheck = ''
                  cabal-fmt hookmark.cabal | diff hookmark.cabal -
                  ./misc/hlintCheck src test hookmark hookmark-web
                   ./misc/formatCheck src test hookmark hookmark-web
                '';
              })
            ;
          });
        };
      };
    };
}
