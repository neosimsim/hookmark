.POSIX:

CMD=stack run --

all: \
	test_help \
	test_version \

test_help: phony
	$(CMD) --help | diff - help.out
	$(CMD) -h | diff - help.out

test_version: phony
	$(CMD) --version >/dev/null
	$(CMD) -v >/dev/null

phony: this_file_should_not_exist

this_file_should_not_exist:
