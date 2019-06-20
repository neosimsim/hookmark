.POSIX:

regress: phony
	make -C regress all

phony: this_file_should_not_exists

this_file_should_not_exists:

