{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if compiler == "default" then haskellPackages else haskell.packages.${compiler};
in
  (hsPkgs.callCabal2nix "hookmark" ./. {}).overrideAttrs (oldAttr: {
    checkInputs = with nixpkgs.haskellPackages; [
      cabal-fmt
      cabal-install
      hlint
      ormolu
    ];
    preCheck = ''make check'';
  })
