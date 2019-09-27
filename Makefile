.POSIX:

STACK_ARGS=

all:
	$(MAKE) build
	$(MAKE) lint
	$(MAKE) test
	$(MAKE) regression

lint: phony
	stack $(STACK_ARGS) exec --package hfmt hfmt

build: phony
	stack $(STACK_ARGS) build --pedantic

test: phony
	stack $(STACK_ARGS) test --pedantic :test

regression: phony
	stack $(STACK_ARGS) --local-bin-path build --verbosity silent install
	stack $(STACK_ARGS) test --pedantic :regression

clean: phony
	rm -rf build
	stack $(STACK_ARGS) clean

check: phony
	./misc/git/pre-commit

phony: this_file_should_not_exists

this_file_should_not_exists:

