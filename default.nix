{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if ghc == "default" then haskellPackages else haskell.packages.${ghc};
in
  (hsPkgs.callCabal2nixWithOptions "hookmark" ./. "-fpedantic" {}).overrideAttrs (oldAttr: {
    checkInputs = with nixpkgs.haskellPackages; [
      cabal-fmt
      cabal-install
      hlint
      ormolu
    ];
    preCheck = ''make check'';
  })
