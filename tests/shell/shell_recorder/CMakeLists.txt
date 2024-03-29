function (add_shell_recorder_test FILENAME)
	set (multiValueArgs REQUIRED_PLUGINS)
	cmake_parse_arguments (ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	get_filename_component (testname_we ${FILENAME} NAME_WE)
	add_test (testshell_${testname_we} "${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh" "${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}")
	set_property (TEST testshell_${testname_we} PROPERTY LABELS memleak kdbtests)
	set_property (TEST testshell_${testname_we} PROPERTY ENVIRONMENT LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib)
	set_property (TEST testshell_${testname_we} PROPERTY RUN_SERIAL TRUE)
endfunction (add_shell_recorder_test FILENAME)

if (ENABLE_KDB_TESTING)

	if (BUILD_SHARED)
		list (FIND REMOVED_PLUGINS list list_index)
		if (list_index EQUAL -1)
			set (ENABLE_REPLAY_TESTS TRUE)
		endif (list_index EQUAL -1)
	endif ()

	set (INCLUDE_COMMON "${INCLUDE_COMMON_FILE}export PATH=\"${CMAKE_BINARY_DIR}/bin:$PATH\"")

	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/shell_recorder.sh" "${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh" @ONLY)
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/tutorial_wrapper/markdown_shell_recorder.sh"
			"${CMAKE_CURRENT_BINARY_DIR}/tutorial_wrapper/markdown_shell_recorder.sh" @ONLY)

	add_shell_recorder_test (db_changes.esr)
	add_shell_recorder_test (hosts.esr REQUIRED_PLUGINS hosts)
	# DISABLED add_shell_recorder_test (listtest.esr REQUIRED_PLUGINS dump list)
	add_shell_recorder_test (mathcheck.esr REQUIRED_PLUGINS math ni)
	add_shell_recorder_test (mv.esr)
	# DISABLED add_shell_recorder_test (profiletest.esr)
	add_shell_recorder_test (script.esr)
	add_shell_recorder_test (selftest.esr REQUIRED_PLUGINS mini)
	add_shell_recorder_test (record.esr REQUIRED_PLUGINS recorder)
	add_shell_recorder_test (record_undo.esr REQUIRED_PLUGINS recorder)

	if (ENABLE_REPLAY_TESTS)
		file (GLOB REPLAY_TESTS replay_tests/*.esr)
		foreach (file ${REPLAY_TESTS})
			get_filename_component (directory ${file} DIRECTORY)
			get_filename_component (name_without_extension ${file} NAME_WE)
			add_test (testshell_replay_${name_without_extension} "${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh" "${file}"
				  "${directory}/${name_without_extension}.epf")
			set_property (TEST testshell_replay_${name_without_extension} PROPERTY LABELS memleak kdbtests)
			set_property (TEST testshell_replay_${name_without_extension} PROPERTY ENVIRONMENT
											       LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib)
			set_property (TEST testshell_replay_${name_without_extension} PROPERTY RUN_SERIAL TRUE)
		endforeach (file ${SCRIPT_TESTS})
	endif (ENABLE_REPLAY_TESTS)

	add_subdirectory (tutorial_wrapper)
endif (ENABLE_KDB_TESTING)
