if (ENABLE_KDB_TESTING)
	# set kdb command
	SET(USE_CMAKE_KDB_COMMAND "")
	if (BUILD_SHARED)
		set (KDB_COMMAND "kdb")
		set (ENABLE_REPLAY_TESTS TRUE)
	elseif (BUILD_FULL)
		set (KDB_COMMAND "kdb-full")
	elseif (BUILD_STATIC)
		set (KDB_COMMAND "kdb-static")
	else()
		message(SEND_ERROR "no kdb tool found, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()

	get_filename_component (PARENT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
	file (COPY "${PARENT_SOURCE_DIR}/include_common.sh.in" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
	configure_file ("${CMAKE_CURRENT_BINARY_DIR}/include_common.sh.in" "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh" @ONLY)

	file (READ "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh" INCLUDE_COMMON_FILE)
	string (LENGTH "${USE_CMAKE_KDB_COMMAND}" LENGTH_KDB_COMMAND)
	if (LENGTH_KDB_COMMAND EQUAL 0)
		set (INCLUDE_COMMON
			"${INCLUDE_COMMON_FILE}\n
			KDB=\"${KDB_COMMAND}\"\n\
			export PATH=\"${CMAKE_BINARY_DIR}/bin\":$PATH"
		)
	else (LENGTH_KDB_COMMAND EQUAL 0)
		set (INCLUDE_COMMON "${INCLUDE_COMMON_FILE}\nKDB=\"kdb\"")
	endif (LENGTH_KDB_COMMAND EQUAL 0)

	configure_file (
			"${CMAKE_CURRENT_SOURCE_DIR}/shell_recorder.sh"
			"${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh"
			@ONLY
		)
	configure_file (
			"${CMAKE_CURRENT_SOURCE_DIR}/tutorial_wrapper/markdown_shell_recorder.sh"
			"${CMAKE_CURRENT_BINARY_DIR}/tutorial_wrapper/markdown_shell_recorder.sh"
			@ONLY
		)

	set (SCRIPT_TESTS
		"db_changes.esr"
		"hosts.esr"
		"listtest.esr"
		"profiletest.esr"
		"script.esr"
	)

	# Only add the tests below if they work using the current configuration
	list (FIND REMOVED_PLUGINS mini PLUGIN_INDEX)
	if (${PLUGIN_INDEX} EQUAL -1)
		list (APPEND SCRIPT_TESTS "selftest.esr")
	endif (${PLUGIN_INDEX} EQUAL -1)

	list (FIND REMOVED_PLUGINS ni PLUGIN_INDEX)
	if (${PLUGIN_INDEX} EQUAL -1)
		list (APPEND SCRIPT_TESTS "mathcheck.esr")
	endif (${PLUGIN_INDEX} EQUAL -1)

	foreach (name ${SCRIPT_TESTS})
		get_filename_component (testname_we ${name} NAME_WE)
		add_test (
			testshell_${testname_we}
			"${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh"
			"${CMAKE_CURRENT_SOURCE_DIR}/${name}"
			)

		set_property(TEST testshell_${testname_we} PROPERTY LABELS memleak kdbtests)
	endforeach (name ${SCRIPT_TESTS})

	if (ENABLE_REPLAY_TESTS)
		file (GLOB REPLAY_TESTS replay_tests/*.esr)
		foreach (file ${REPLAY_TESTS})
			get_filename_component (directory ${file} DIRECTORY)
			get_filename_component (name_without_extension ${file} NAME_WE)
			add_test (
				testshell_replay_${name_without_extension}
				"${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh"
				"${file}"
				"${directory}/${name_without_extension}.epf"
				)
			set_property(TEST testshell_replay_${name_without_extension} PROPERTY LABELS memleak kdbtests)
		endforeach (file ${SCRIPT_TESTS})
	endif (ENABLE_REPLAY_TESTS)

	add_subdirectory (tutorial_wrapper)
endif (ENABLE_KDB_TESTING)
