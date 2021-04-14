{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if ghc == "default"
    then haskellPackages
    else haskell.packages.${ghc};
in
{
  haskellPackages = hsPkgs.override {
    overrides = (self: super: {
      hookmark = import ./. { inherit nixpkgs ghc; };
    });
  };
}

