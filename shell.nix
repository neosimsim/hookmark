{ nixpkgs ? import <nixpkgs> {}, ghc ? "default" }:
let
  packages = import ./packages.nix { inherit nixpkgs ghc; };
in
  packages.haskellPackages.shellFor {
    packages = p: [ p.hookmark ];
    withHoogle = true;
    buildInputs = with packages.haskellPackages; [
      ormolu
    ];
  }

