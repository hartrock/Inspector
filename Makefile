SHELL=/bin/bash # avoid using /bin/dash here
MOD_TEST_FILES := Util\:string-to-sym.lsp
MOD_TEST_FILES_TESTED := $(patsubst %,tst/%_tested, $(MOD_TEST_FILES))

all_tests:
	make mod_tests 2>/dev/null | grep '>>>>>>'
# deps
mod_tests: $(MOD_TEST_FILES_TESTED)
lib_tests:
	@echo 'todo'

# rules
tst/%.lsp_tested: tst/%.lsp
	newlisp $<

show_vars:
	@echo "MOD_TEST_FILES: $(MOD_TEST_FILES)"
	@echo "MOD_TEST_FILES_TESTED: $(MOD_TEST_FILES_TESTED)"
test_shell:
	@echo "$$ (SHELL): $(SHELL)"
	ls -l /bin/sh
	#env
TTT:
# EOF
