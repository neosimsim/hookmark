.POSIX:

CABAL_BUILD_ARGS=
TOOL_GHC=ghc-8.6.5

all:
	$(MAKE) build
	$(MAKE) test
	$(MAKE) regression

check: phony tools/cabal-fmt tools/hfmt
	tools/cabal-fmt hookmark.cabal | diff hookmark.cabal -
	tools/hfmt

check-apply: phony tools/cabal-fmt tools/hfmt
	tools/cabal-fmt -i hookmark.cabal
	tools/hfmt -w

build: phony
	cabal v2-build -f pedantic $(CABAL_BUILD_ARGS)

test: phony
	cabal v2-run -f pedantic $(CABAL_BUILD_ARGS) test -- $(HSPEC_ARGS)

regression: phony
	cabal v2-install -f pedantic --installdir build --install-method copy --overwrite-policy always $(CABAL_BUILD_ARGS)
	cabal v2-run -f pedantic $(CABAL_BUILD_ARGS) regression -- $(HSPEC_ARGS)

yesod: phony tools/yesod
	tools/yesod devel

tools: tools/hfmt tools/cabal-fmt tools/yesod

tools/hfmt:
	# Unable to install hfmt with haskell-src-exts 1.22
	# https://github.com/chrisdone/hindent/issues/562
	# Also not installable with ghc >8.6.5.
	mkdir -p tools
	cd tools && cabal v2-install -w $(TOOL_GHC) --installdir . --install-method copy --constraint 'haskell-src-exts < 1.22' hfmt-0.2.3.1

tools/cabal-fmt:
	mkdir -p tools
	cd tools && cabal v2-install -w $(TOOL_GHC) --installdir . --install-method copy cabal-fmt-0.1.2

tools/yesod:
	mkdir -p tools
	cd tools && cabal v2-install -w $(TOOL_GHC) --installdir . --install-method copy yesod-bin

clean: phony
	rm -rf build tools
	cabal v2-clean

phony: this_file_should_not_exists

this_file_should_not_exists:

