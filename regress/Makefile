.POSIX:

CMD=stack run --

all: \
	test_help \
	test_help_add \
	test_help_show \
	test_help_edit \
	test_add \
	test_show \
	test_edit \
	test_version \

test_help: phony
	$(CMD) --help | diff - help.out
	$(CMD) -h | diff - help.out

test_help_add: phony
	$(CMD) add --help | diff - help_add.out
	$(CMD) add -h | diff - help_add.out

test_help_show: phony
	$(CMD) show --help | diff - help_show.out
	$(CMD) show -h | diff - help_show.out

test_help_edit: phony
	$(CMD) edit --help | diff - help_edit.out
	$(CMD) edit -h | diff - help_edit.out

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

test_show: phony
	@echo "test if show list directories from the correct place"
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show | diff - show_all.out
	@echo "option shall overwrite environment"
	export HOOKMARKHOME=/tmp; base="`pwd`/testmarks"; $(CMD) show -b $$base | diff - show_all.out
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show -t search | diff - show_tag_search.out
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show -t search --tag haskell | diff - testmarks/haskell/hoogle
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show haskell/hoogle | diff - testmarks/haskell/hoogle
	@echo "returns non successful, if bookmark not found"
	export HOOKMARKHOME="`pwd`/testmarks"; if $(CMD) show missing/bookmark 2>&1 >/dev/null; then exit 1; fi | diff - show_not_found.out
	@echo "returns non successful, if tag is not found"
	export HOOKMARKHOME="`pwd`/testmarks"; if $(CMD) show -t missingtag 2>&1 >/dev/null; then exit 1; fi | diff - show_not_found.out
	@echo "returns non successful, if tag is missing in bookamrk, like not found"
	export HOOKMARKHOME="`pwd`/testmarks"; if $(CMD) show -t missingtag haskell/hoogle 2>&1 >/dev/null; then exit 1; fi | diff - show_not_found.out

test_edit: phony
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME";\
		export EDITOR=ed;\
		$(CMD) edit haskell/hoogle <edit.in >/dev/null;\
		diff "$$HOOKMARKHOME"/haskell/hoogle edit.out;\
		rm -rf "$$HOOKMARKHOME"

phony: this_file_should_not_exist

this_file_should_not_exist:
