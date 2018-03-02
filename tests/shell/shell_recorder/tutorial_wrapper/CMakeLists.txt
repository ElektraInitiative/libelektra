include (LibAddTest)

add_s_test (msr_syntax "${CMAKE_SOURCE_DIR}/tests/shell/shell_recorder/tutorial_wrapper/SyntaxCheck.md" REQUIRED_PLUGINS ini)
add_s_test (ini_crash_test "${CMAKE_SOURCE_DIR}/src/plugins/ini/crash_test/crash_test.md" REQUIRED_PLUGINS ini)
add_s_test (kdb-global-umount "${CMAKE_SOURCE_DIR}/doc/help/kdb-global-umount.md" REQUIRED_PLUGINS spec)

add_s_test (readme_msr "${CMAKE_SOURCE_DIR}/tests/shell/shell_recorder/tutorial_wrapper/README.md")
add_s_test (issue_template "${CMAKE_SOURCE_DIR}/.github/ISSUE_TEMPLATE.md")
add_s_test (tutorial_cascading "${CMAKE_SOURCE_DIR}/doc/tutorials/cascading.md")
add_s_test (kdb-complete "${CMAKE_SOURCE_DIR}/doc/help/kdb-complete.md")
add_s_test (kdb-ls "${CMAKE_SOURCE_DIR}/doc/help/kdb-ls.md")

# The tests below might fail on Linux if ASAN is enabled
# The problem might be related to a call of a function via a pointer of incorrect type
# See also: https://github.com/ElektraInitiative/libelektra/pull/1728
set (ASAN_LINUX ENABLE_ASAN AND ${CMAKE_SYSTEM_NAME} MATCHES "Linux")
if (NOT (${ASAN_LINUX} AND CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4))
	add_s_test (tutorial_validation "${CMAKE_SOURCE_DIR}/doc/tutorials/validation.md" REQUIRED_PLUGINS validation)
	add_plugin_shell_test (base64)
	add_plugin_shell_test (ini)
	add_plugin_shell_test (mini)
	add_plugin_shell_test (tcl)
endif (NOT (${ASAN_LINUX} AND CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4))

# Only works with super user privileges, since it writes to `/etc/hosts`:
# add_s_test (tutorial_mount "${CMAKE_SOURCE_DIR}/doc/tutorials/mount.md")
