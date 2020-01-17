{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
with nixpkgs.pkgs.haskell;
lib.justStaticExecutables (packages.${compiler}.callPackage ./hookmark.nix { } )
