{ nixpkgs ? import <nixpkgs> { }, ghc ? "default" }:
let
  hsPkgs = with nixpkgs.pkgs; if ghc == "default" then haskellPackages else haskell.packages.${ghc};
in
(hsPkgs.callCabal2nixWithOptions "hookmark" ./. "-fpedantic" { }).overrideAttrs (oldAttr: {
  nativeBuildInputs = oldAttr.nativeBuildInputs ++ [ final.installShellFiles ];

  checkInputs = with nixpkgs.haskellPackages; [
    cabal-fmt
    cabal-install
    hlint
    ormolu
  ];
  preCheck = ''make check'';

  postInstall = ''
    installShellCompletion --cmd hookmark \
      --bash <($out/bin/hookmark --bash-completion-srcipt $out/bin/hookmark) \
      --fish <($out/bin/hookmark --fish-completion-script $out/bin/hookmark) \
      --zsh <($out/bin/hookmark --zsh-completion-script $out/bin/hookmark)

    installShellCompletion --cmd hookmark-web \
      --bash <($out/bin/hookmark-web --bash-completion-srcipt $out/bin/hookmark-web) \
      --fish <($out/bin/hookmark-web --fish-completion-script $out/bin/hookmark-web) \
      --zsh <($out/bin/hookmark-web --zsh-completion-script $out/bin/hookmark-web)
  '';
})
