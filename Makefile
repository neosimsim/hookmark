.POSIX:

CABAL_BUILD_ARGS=
TOOL_GHC=ghc-8.10.4

all:
	$(MAKE) build
	$(MAKE) test
	$(MAKE) regression

check: phony tools/cabal-fmt tools/brittany tools/stylish-haskell tools/hlint tools/refactor
	cabal check
	tools/cabal-fmt hookmark.cabal | diff hookmark.cabal -
	PATH=tools:$$PATH ./misc/hlintCheck src test hookmark hookmark-web
	PATH=tools:$$PATH ./misc/formatCheck src test hookmark hookmark-web

check-apply: phony tools/cabal-fmt tools/brittany tools/stylish-haskell tools/hlint tools/refactor
	tools/cabal-fmt -i hookmark.cabal
	PATH=tools:$$PATH ./misc/hlintApply src test hookmark hookmark-web
	PATH=tools:$$PATH ./misc/formatApply src test hookmark hookmark-web

build: phony
	cabal v2-build -f pedantic $(CABAL_BUILD_ARGS)

test: phony
	cabal v2-run -f pedantic $(CABAL_BUILD_ARGS) test -- $(HSPEC_ARGS)

regression: phony
	cabal v2-install -f pedantic --installdir build --install-method copy --overwrite-policy always $(CABAL_BUILD_ARGS)
	cabal v2-run -f pedantic $(CABAL_BUILD_ARGS) regression -- $(HSPEC_ARGS)

yesod: phony tools/yesod
	tools/yesod devel

tools: tools/cabal-fmt tools/yesod tools/stylish-haskell tools/brittany tools/hlint tools/refactor

tools/cabal-fmt:
	mkdir -p tools
	cd tools && cabal v2-install --ghc-options -j6 -w $(TOOL_GHC) --installdir . --install-method copy cabal-fmt-0.1.5.1

tools/yesod:
	mkdir -p tools
	cd tools && cabal v2-install --ghc-options -j6 -w $(TOOL_GHC) --installdir . --install-method copy yesod-bin

tools/stylish-haskell:
	mkdir -p tools
	cd tools && cabal v2-install --ghc-options -j6 -w $(TOOL_GHC) --installdir . --install-method copy stylish-haskell-0.12.2.0

tools/brittany:
	mkdir -p tools
	cd tools && cabal v2-install --ghc-options -j6 -w $(TOOL_GHC) --installdir . --install-method copy brittany-0.13.1.0

tools/hlint:
	mkdir -p tools
	cd tools && cabal v2-install --ghc-options -j6 -w $(TOOL_GHC) --installdir . --install-method copy hlint-3.3

tools/refactor:
	mkdir -p tools
	cd tools && cabal v2-install --ghc-options -j6 -w $(TOOL_GHC) --installdir . --install-method copy apply-refact-0.9.2.0

clean: phony
	rm -rf build tools
	cabal v2-clean

phony: this_file_should_not_exists

this_file_should_not_exists:

