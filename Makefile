SHELL=/bin/bash # avoid using /bin/dash here
MOD_TEST_FILES := Util\:string-to-sym.lsp
MOD_TEST_FILES_TESTED := $(patsubst %,tst/%_tested, $(MOD_TEST_FILES))
LIB_TEST_FILES := WS.lsp
LIB_TEST_FILES_TESTED := $(patsubst %,tst/%_tested, $(LIB_TEST_FILES))

# targets
all_tests:
	make all_checks | grep '>>>>>>'
all_checks:
	make all_debug 2>/dev/null
all_debug:
	make mod_tests
	make lib_tests

# deps
mod_tests: $(MOD_TEST_FILES_TESTED)
lib_tests: $(LIB_TEST_FILES_TESTED)

# rules
tst/%.lsp_tested: tst/%.lsp
	newlisp $<

show_vars:
	@echo "MOD_TEST_FILES: $(MOD_TEST_FILES)"
	@echo "MOD_TEST_FILES_TESTED: $(MOD_TEST_FILES_TESTED)"
	@echo "LIB_TEST_FILES: $(LIB_TEST_FILES)"
	@echo "LIB_TEST_FILES_TESTED: $(LIB_TEST_FILES_TESTED)"
test_shell:
	@echo "<> ls -l /bin/sh"
	@echo ">> "`ls -l /bin/sh`
	@echo "$$""(SHELL): $(SHELL)"
	@ #env
TTT:
# EOF
