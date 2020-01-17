{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hookmark.nix { }
