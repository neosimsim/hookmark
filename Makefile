.POSIX:

all: lint test
	$(MAKE) regress

lint: phony
	hfmt

build: phony
	stack build --pedantic

test: build
	stack test --pedantic

regress: build
	$(MAKE) -C regress all

clean:
	stack clean

check:
	./misc/git/pre-commit

phony: this_file_should_not_exists

this_file_should_not_exists:

