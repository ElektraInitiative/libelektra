include (LibAddTest)

add_msr_test (msr_syntax "${CMAKE_SOURCE_DIR}/tests/shell/shell_recorder/tutorial_wrapper/SyntaxCheck.md" REQUIRED_PLUGINS ini)
add_msr_test (ini_crash_test "${CMAKE_SOURCE_DIR}/src/plugins/ini/crash_test/crash_test.md" REQUIRED_PLUGINS ini)
add_msr_test (kdb_global_umount "${CMAKE_SOURCE_DIR}/doc/help/kdb-global-umount.md" REQUIRED_PLUGINS spec tracer)

add_msr_test (readme_msr "${CMAKE_SOURCE_DIR}/tests/shell/shell_recorder/tutorial_wrapper/README.md")
add_msr_test (issue_template "${CMAKE_SOURCE_DIR}/.github/ISSUE_TEMPLATE.md")
add_msr_test (tutorial_cascading "${CMAKE_SOURCE_DIR}/doc/tutorials/cascading.md")
add_msr_test (kdb_complete "${CMAKE_SOURCE_DIR}/doc/help/kdb-complete.md")
add_msr_test (kdb_ls "${CMAKE_SOURCE_DIR}/doc/help/kdb-ls.md" REQUIRED_PLUGINS sync)

if (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9)
	message (STATUS "Excluding Markdown Shell Recorder test for `kdb find`, since GCC ${CMAKE_CXX_COMPILER_VERSION} "
			"does not support regular expressions properly")
else (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9)
	add_msr_test (kdb_find "${CMAKE_SOURCE_DIR}/doc/help/kdb-find.md" REQUIRED_PLUGINS sync)
endif (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9)

if (ENABLE_ASAN)
	message (STATUS "Excluding Markdown Shell Recorder test for `validation`, as it leaks memory and fails with ASAN enabled")
else (ENABLE_ASAN)
	add_msr_test (tutorial_validation "${CMAKE_SOURCE_DIR}/doc/tutorials/validation.md" REQUIRED_PLUGINS ni validation)
endif (ENABLE_ASAN)
# ~~~
# Only works with super user privileges, since it writes to `/etc/hosts`:
# add_msr_test (tutorial_mount "${CMAKE_SOURCE_DIR}/doc/tutorials/mount.md")
# ~~~
