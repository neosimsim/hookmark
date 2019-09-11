.POSIX:

CMD=stack run --

all: \
	test_help \
	test_help_add \
	test_help_show \
	test_help_edit \
	test_help_rm \
	test_help_mv \
	test_help_git \
	test_add \
	test_show \
	test_edit \
	test_rm \
	test_mv \
	test_git \
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

test_help_rm: phony
	$(CMD) rm --help | diff - help_rm.out
	$(CMD) rm -h | diff - help_rm.out

test_help_mv: phony
	$(CMD) mv --help | diff - help_mv.out
	$(CMD) mv -h | diff - help_mv.out

test_help_git: phony
	$(CMD) git --help | diff - help_git.out
	$(CMD) git -h | diff - help_git.out

test_version: phony
	$(CMD) --version >/dev/null
	$(CMD) -v >/dev/null
	$(CMD) version >/dev/null

test_add: phony
	@echo "test if add create files and the correct place"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME &&\
		$(CMD) add -t search --tag haskell haskell/hoogle https://hoogle.haskell.org/ &&\
		test -d "$$HOOKMARKHOME/haskell" &&\
		test -f "$$HOOKMARKHOME/haskell/hoogle" &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "base dir shall be created"
	export HOOKMARKHOME="`mktemp -ud /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		$(CMD) add -t search --tag haskell haskell/hoogle https://hoogle.haskell.org/ &&\
		test -d "$$HOOKMARKHOME/haskell" &&\
		test -f "$$HOOKMARKHOME/haskell/hoogle" &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "option shall overwrite environment"
	export HOOKMARKHOME=/tmp;\
		export base="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo base=$$base;\
		$(CMD) add -b "$$base" -t search --tag haskell haskell/hoogle https://hoogle.haskell.org/ &&\
		test -d "$$base/haskell" &&\
		test -f "$$base/haskell/hoogle" &&\
		rm -rf "$$base"
	@echo "add shall use hookmark root, not file system root"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		$(CMD) add -t search --tag haskell /haskell/hoogle https://hoogle.haskell.org/ &&\
		test -d "$$HOOKMARKHOME/haskell" &&\
		test -f "$$HOOKMARKHOME/haskell/hoogle" &&\
		rm -rf "$$HOOKMARKHOME"

test_show: phony
	@echo "test if show list directories from the correct place"
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show | diff - show_all.out
	@echo "test if show list hookmark root, not file system root"
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show / | diff - show_all.out
	export HOOKMARKHOME="`pwd`/testmarks"; $(CMD) show /haskell/hoogle | diff - testmarks/haskell/hoogle
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
		$(CMD) edit haskell/hoogle <edit.in >/dev/null &&\
		diff "$$HOOKMARKHOME"/haskell/hoogle edit.out &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "edit shall use hookmark root, not file system root"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME";\
		export EDITOR=ed;\
		$(CMD) edit /haskell/hoogle <edit.in >/dev/null &&\
		diff "$$HOOKMARKHOME"/haskell/hoogle edit.out &&\
		rm -rf "$$HOOKMARKHOME"

test_rm: phony
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_rm_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) rm haskell/packages/warp &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages/warp" ]; then echo "expected bookmark to be removed" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "rm shall remove empty folders"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_rm_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) rm haskell/packages/warp &&\
		$(CMD) rm haskell/packages/base64-bytestring &&\
		if [ -e "$$HOOKMARKHOME/haskell/package" ]; then echo "expected folder to be removed" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "rm shall not remove empty base"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_rm_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) rm haskell/hoogle &&\
		$(CMD) rm haskell/haskell &&\
		$(CMD) rm haskell/packages/warp &&\
		$(CMD) rm haskell/packages/base64-bytestring &&\
		$(CMD) rm duckduckgo &&\
		if ! [ -e "$$HOOKMARKHOME/" ]; then echo "expected root not to be removed" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "rm shall use hookmark root, not file system root"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_rm_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) rm /duckduckgo &&\
		if [ -e "$$HOOKMARKHOME/duckduckgo" ]; then echo "expected folder to be removed" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"

test_mv: phony
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp haskell &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -e "$$HOOKMARKHOME/haskell/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp haskell/packages/warp2 &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -e "$$HOOKMARKHOME/haskell/packages/warp2" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp haskell/packages/base64-bytestring &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages/warp" ]; then echo "expected bookmark to be removed" >&2; exit 1; fi &&\
		if ! [ -e "$$HOOKMARKHOME/haskell/packages/base64-bytestring" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall remove empty folders"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp haskell &&\
		$(CMD) mv haskell/packages/base64-bytestring haskell &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages" ]; then echo "expected folder to be removed" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall use hookmark root, not file system root"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp /haskell &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -e "$$HOOKMARKHOME/haskell/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp / &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -e "$$HOOKMARKHOME/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall move folders"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/ / &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -d "$$HOOKMARKHOME/packages" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall move list of files simultanously"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		$(CMD) mv haskell/packages/warp haskell/packages/base64-bytestring / &&\
		if ! [ -f "$$HOOKMARKHOME/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -f "$$HOOKMARKHOME/base64-bytestring" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if [ -e "$$HOOKMARKHOME/haskell/packages" ]; then echo "expected empty folder to be removed" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall report missing arguments"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		if $(CMD) mv; then exit 1; fi 2>&1 | diff - mv_missing_arg.out &&\
		if $(CMD) mv haskell/packages/warp; then exit 1; fi 2>&1 | diff - mv_missing_arg.out &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall report missing folder when list of files shall be moved"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		if $(CMD) mv haskell/packages/warp duckduckgo haskell/packages/base64-bytestring; then exit 1; fi 2>&1 | diff - mv_missing_folder.out &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		if $(CMD) mv haskell/packages/warp duckduckgo haskell/packages/base64-bytestring /not-existing; then exit 1; fi 2>&1 | diff - mv_missing_folder_2.out &&\
		rm -rf "$$HOOKMARKHOME"
	@echo "mv shall report missing files if content and folder are moved at the same time"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_mv_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		if $(CMD) mv haskell/packages/warp haskell/packages haskell/packages/base64-bytestring haskell/hoogle / 2>&1;\
		then\
			exit 1;\
		fi | diff - mv_missing_file.out &&\
		if ! [ -f "$$HOOKMARKHOME/warp" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -d "$$HOOKMARKHOME/packages" ]; then echo "expected folder to be moved" >&2; exit 1; fi &&\
		if ! [ -f "$$HOOKMARKHOME/packages/base64-bytestring" ]; then echo "expected bookmark to be moved" >&2; exit 1; fi &&\
		if ! [ -f "$$HOOKMARKHOME/haskell/hoogle" ]; then echo "expected bookmark not to be moved" >&2; exit 1; fi &&\
		rm -rf "$$HOOKMARKHOME"

test_git:
	@echo "git shall execute git from PATH"
	export PATH="`pwd`:$$PATH"; which git
	export PATH="`pwd`:$$PATH"; $(CMD) git push | diff - git.out
	@echo "test auto commit"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME &&\
		export PATH="`pwd`:$$PATH";\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		mkdir "$$HOOKMARKHOME/.git" &&\
		$(CMD) add --tag haskell haskell/fpcomplete https://haskell.fpcomplete.com | diff - git_add.out &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME &&\
		export PATH="`pwd`:$$PATH";\
		export EDITOR=ed;\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		mkdir "$$HOOKMARKHOME/.git" &&\
		$(CMD) edit haskell/hoogle <edit.in | diff - git_edit.out &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME &&\
		export PATH="`pwd`:$$PATH";\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		mkdir "$$HOOKMARKHOME/.git" &&\
		$(CMD) rm duckduckgo | diff - git_rm.out &&\
		rm -rf "$$HOOKMARKHOME"
	export HOOKMARKHOME="`mktemp -d /tmp/hookmark_test_add_XXXXXX`";\
		echo HOOKMARKHOME=$$HOOKMARKHOME &&\
		export PATH="`pwd`:$$PATH";\
		cp -r testmarks/* "$$HOOKMARKHOME" &&\
		mkdir "$$HOOKMARKHOME/.git" &&\
		touch "$$HOOKMARKHOME/.git/phony-git-file" &&\
		mkdir "$$HOOKMARKHOME/haskell/.git" &&\
		touch "$$HOOKMARKHOME/haskell/.git/phony-git-file" &&\
		$(CMD) show | diff - git_show.out &&\
		rm -rf "$$HOOKMARKHOME"

phony: this_file_should_not_exist

this_file_should_not_exist:
