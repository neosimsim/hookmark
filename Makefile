.POSIX:

CABAL_BUILD_ARGS=
TOOL_GHC=ghc-8.10.4

all:
	$(MAKE) build
	$(MAKE) test
	$(MAKE) regression

check: phony
	cabal check
	cabal-fmt hookmark.cabal | diff hookmark.cabal -
	./misc/hlintCheck src test hookmark hookmark-web
	./misc/formatCheck src test hookmark hookmark-web

check-apply: phony
	cabal-fmt -i hookmark.cabal
	./misc/hlintApply src test hookmark hookmark-web
	./misc/formatApply src test hookmark hookmark-web

build: phony
	cabal v2-build -f pedantic $(CABAL_BUILD_ARGS)

test: phony
	cabal v2-run -f pedantic $(CABAL_BUILD_ARGS) test -- $(HSPEC_ARGS)

regression: phony
	mkdir -p dist/build
	cabal v2-install -f pedantic --installdir dist/build/hookmark --install-method copy --overwrite-policy always $(CABAL_BUILD_ARGS) exe:hookmark
	cabal v2-install -f pedantic --installdir dist/build/hookmark-web --install-method copy --overwrite-policy always $(CABAL_BUILD_ARGS) exe:hookmark-web
	cabal v2-run -f pedantic $(CABAL_BUILD_ARGS) regression -- $(HSPEC_ARGS)

clean: phony
	rm -rf build tools
	cabal v2-clean

phony: this_file_should_not_exists

this_file_should_not_exists:

