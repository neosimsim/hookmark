with (import <nixpkgs> {});
derivation {
  name = "hookmark";
  src = ./.;
  builder = "${cabal-install}/bin/cabal";
  args = ["install" "--prefix" "$out"];
  system = builtins.currentSystem;
}
