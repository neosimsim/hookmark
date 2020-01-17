{ mkDerivation, base, bytestring, containers, Diff, directory
, either, file-embed, filepath, hspec, hspec-contrib
, hspec-expectations, HUnit, megaparsec, nicify-lib, non-empty-text
, optparse-applicative, process, QuickCheck, raw-strings-qq
, shakespeare, stdenv, template-haskell, temporary, text
, typed-process, unix, unliftio, utf8-string, yesod
}:
mkDerivation {
  pname = "hookmark";
  version = "1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory either filepath megaparsec
    non-empty-text process shakespeare template-haskell text
    typed-process utf8-string yesod
  ];
  executableHaskellDepends = [
    base bytestring directory filepath non-empty-text
    optparse-applicative raw-strings-qq template-haskell temporary text
    typed-process yesod
  ];
  testHaskellDepends = [
    base bytestring Diff directory either file-embed filepath hspec
    hspec-contrib hspec-expectations HUnit megaparsec nicify-lib
    non-empty-text process QuickCheck template-haskell text
    typed-process unix unliftio utf8-string
  ];
  homepage = "https://gitlab.com/neosimsim/hookmark";
  description = "Browser independent bookmark manager";
  license = stdenv.lib.licenses.bsd3;
}
