.POSIX:

all: lint build test regress

lint: phony
	hfmt

build: phony
	stack build --pedantic

test: build
	stack test --pedantic

regress: build
	make -C regress all

clean:
	stack clean

check:
	./misc/git/pre-commit

phony: this_file_should_not_exists

this_file_should_not_exists:

