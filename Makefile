.POSIX:

all:
	$(MAKE) build
	$(MAKE) lint
	$(MAKE) test
	$(MAKE) regression

lint: phony
	stack exec --package hfmt hfmt

build: phony
	stack build --pedantic

test: phony
	stack test --pedantic :test

regression: phony
	stack --local-bin-path build --verbosity silent install
	stack test --pedantic :regression

clean: phony
	rm -rf build
	stack clean

check: phony
	./misc/git/pre-commit

phony: this_file_should_not_exists

this_file_should_not_exists:

