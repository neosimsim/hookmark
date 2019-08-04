.POSIX:

all: lint test
	$(MAKE) regression

lint: phony
	hfmt

build: phony
	stack build --pedantic

test: build
	stack test --pedantic :test

regression: build
	stack test --pedantic :regression

clean:
	stack clean

check:
	./misc/git/pre-commit

phony: this_file_should_not_exists

this_file_should_not_exists:

