.POSIX:

CMD=stack run --

all: \
	test_help \
	test_help_add \
	test_add \
	test_version \

test_help: phony
	$(CMD) --help | diff - help.out
	$(CMD) -h | diff - help.out

test_help_add: phony
	$(CMD) add --help | diff - help_add.out
	$(CMD) add -h | diff - help_add.out

test_version: phony
	$(CMD) --version >/dev/null
	$(CMD) -v >/dev/null
	$(CMD) version >/dev/null

test_add: phony
	@echo "test if add create files and the correct place"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		$(CMD) add -t search\ --tag haskell\ haskell/hoogle https://hoogle.haskell.org/\
			&& test -d "$$HOOKMARKHOME/haskell"\
			&& test -f "$$HOOKMARKHOME/haskell/hoogle";\
		rm -rf "$$HOOKMARKHOME"
	@echo "base dir shall be created"
	export HOOKMARKHOME="`mktemp -ud /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		$(CMD) add -t search --tag haskell haskell/hoogle https://hoogle.haskell.org/\
			&& test -d "$$HOOKMARKHOME/haskell"\
			&& test -f "$$HOOKMARKHOME/haskell/hoogle";\
		rm -rf "$$HOOKMARKHOME"
	@echo "option shall overwrite environment"
	export HOOKMARKHOME=/tmp;\
		base="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo base=$$base;\
		$(CMD) add -b "$$base" -t search --tag haskell haskell/hoogle https://hoogle.haskell.org/\
			&& test -d "$$base/haskell"\
			&& test -f "$$base/haskell/hoogle";\
		rm -rf "$$base"

phony: this_file_should_not_exist

this_file_should_not_exist:
