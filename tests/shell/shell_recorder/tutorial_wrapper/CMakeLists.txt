function (add_s_test NAME FILE)
	add_test (
		testshell_markdown_${NAME}
		"${CMAKE_CURRENT_BINARY_DIR}/markdown_shell_recorder.sh"
		"${FILE}"
		)

	set_property(TEST testshell_markdown_${NAME} PROPERTY LABELS memleak kdbtests)
endfunction ()

function (add_plugin_shell_test PLUGIN)
	list (FIND REMOVED_PLUGINS ${PLUGIN} INDEX)
	if (${INDEX} EQUAL -1)
		add_s_test (${PLUGIN} "${CMAKE_SOURCE_DIR}/src/plugins/${PLUGIN}/README.md")
	endif ()
endfunction ()

add_s_test (tutorial_cascading "${CMAKE_SOURCE_DIR}/doc/tutorials/cascading.md")
add_s_test (kdb-complete "${CMAKE_SOURCE_DIR}/doc/help/kdb-complete.md")

add_plugin_shell_test (blockresolver)
add_plugin_shell_test (conditionals)
add_plugin_shell_test (enum)
add_plugin_shell_test (hosts)
add_plugin_shell_test (line)
add_plugin_shell_test (mathcheck)
add_plugin_shell_test (multifile)
add_plugin_shell_test (mini)
add_plugin_shell_test (mozprefs)
add_plugin_shell_test (xerces)
add_plugin_shell_test (range)
add_plugin_shell_test (type)

# Only works with super user privileges, since it writes to `/etc/hosts`:
# add_s_test (tutorial_mount "${CMAKE_SOURCE_DIR}/doc/tutorials/mount.md")
