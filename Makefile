.POSIX:

STACK_ARGS=

all:
	$(MAKE) build
	$(MAKE) check
	$(MAKE) test
	$(MAKE) regression

check: phony
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

phony: this_file_should_not_exists

this_file_should_not_exists:

