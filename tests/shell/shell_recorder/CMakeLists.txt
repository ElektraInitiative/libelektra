if (ENABLE_KDB_TESTING)
	# set kdb command
	SET(USE_CMAKE_KDB_COMMAND "")
	if (BUILD_SHARED)
		set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/kdb")
	elseif (BUILD_FULL)
		set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/kdb-full")
	elseif (BUILD_STATIC)
		set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/kdb-static")
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
			KDBCOMMAND=\"${KDB_COMMAND}\"\n\
			export PATH=\"`dirname \\\"$KDBCOMMAND\\\"`:$PATH\""
		)
	else (LENGTH_KDB_COMMAND EQUAL 0)
		set (INCLUDE_COMMON "${INCLUDE_COMMON_FILE}\nKDBCOMMAND=\"kdb\"")
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

	file (GLOB SCRIPT_TESTS *.esr)
	foreach (file ${SCRIPT_TESTS})
		get_filename_component (name ${file} NAME)
		get_filename_component (testname_we ${name} NAME_WE)
		add_test (
			testshell_${testname_we}
			"${CMAKE_CURRENT_BINARY_DIR}/shell_recorder.sh"
			"${CMAKE_CURRENT_SOURCE_DIR}/${name}"
			)

		set_property(TEST testshell_${testname_we} PROPERTY LABELS memleak kdbtests)
	endforeach (file ${SCRIPT_TESTS})

	add_subdirectory (tutorial_wrapper)
endif (ENABLE_KDB_TESTING)
