include (LibAddTest)

add_msr_test (msr_syntax "${CMAKE_SOURCE_DIR}/tests/shell/shell_recorder/tutorial_wrapper/SyntaxCheck.md" REQUIRED_PLUGINS ini)
add_msr_test (ini_crash_test "${CMAKE_SOURCE_DIR}/src/plugins/ini/crash_test/crash_test.md" REQUIRED_PLUGINS ini)
add_msr_test (kdb-global-umount "${CMAKE_SOURCE_DIR}/doc/help/kdb-global-umount.md" REQUIRED_PLUGINS spec tracer)

add_msr_test (readme_msr "${CMAKE_SOURCE_DIR}/tests/shell/shell_recorder/tutorial_wrapper/README.md")
add_msr_test (issue_template "${CMAKE_SOURCE_DIR}/.github/ISSUE_TEMPLATE.md")
add_msr_test (tutorial_cascading "${CMAKE_SOURCE_DIR}/doc/tutorials/cascading.md")
add_msr_test (kdb-complete "${CMAKE_SOURCE_DIR}/doc/help/kdb-complete.md")
add_msr_test (kdb-ls "${CMAKE_SOURCE_DIR}/doc/help/kdb-ls.md" REQUIRED_PLUGINS sync)

add_msr_test (tutorial_validation "${CMAKE_SOURCE_DIR}/doc/tutorials/validation.md" REQUIRED_PLUGINS validation)

# ~~~
# Only works with super user privileges, since it writes to `/etc/hosts`:
# add_msr_test (tutorial_mount "${CMAKE_SOURCE_DIR}/doc/tutorials/mount.md")
# ~~~
