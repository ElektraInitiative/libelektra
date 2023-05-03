function (add_externaltest testname)
	get_filename_component (testname_we ${testname} NAME_WE)

	set (filename ${CMAKE_CURRENT_SOURCE_DIR}/${testname})
	if (NOT EXISTS ${filename})
		message (SEND_ERROR "add_scripttest: given file ${filename} does not exists")
	endif (NOT EXISTS ${filename})

	if (ENABLE_KDB_TESTING AND INSTALL_TESTING)
		if (BUILD_FULL)
			set (KDB_COMMAND "${CMAKE_INSTALL_PREFIX}/bin/kdb-full")
		elseif (BUILD_STATIC)
			set (KDB_COMMAND "${CMAKE_INSTALL_PREFIX}/bin/kdb-static")
		elseif (BUILD_SHARED)
			set (KDB_COMMAND "${CMAKE_INSTALL_PREFIX}/bin/kdb")
		else ()
			message (SEND_ERROR "no kdb tool found, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
		endif (BUILD_FULL)
		set (INCLUDE_COMMON "${INCLUDE_COMMON_FILE}KDB=\"${KDB_COMMAND}\"")
		set (testscriptname "${CMAKE_CURRENT_BINARY_DIR}/${testname}")
		configure_file ("${filename}" "${testscriptname}" @ONLY)
		# don't actually add as test, only run via `kdb run_all`

		# FIXME: re-enable tests

		#[[
		 install (
		 	FILES "${CMAKE_CURRENT_BINARY_DIR}/${testname}"
		 	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
		 	PERMISSIONS
		 		OWNER_READ
		 		OWNER_WRITE
		 		OWNER_EXECUTE
		 		GROUP_READ
		 		GROUP_EXECUTE
		 		WORLD_READ
		 		WORLD_EXECUTE
		 	RENAME "check_external_${testname_we}"
		 	COMPONENT elektra-tests)
		]]
	endif (ENABLE_KDB_TESTING AND INSTALL_TESTING)
endfunction (add_externaltest)

file (GLOB SCRIPT_TESTS *.sh)
foreach (file ${SCRIPT_TESTS})
	get_filename_component (name ${file} NAME)
	if (name STREQUAL "mount_java.sh")
		if (NOT BUILD_SHARED
		    OR NOT "jna" IN_LIST ADDED_BINDINGS
		    OR NOT "process" IN_LIST ADDED_PLUGINS)
			continue ()
		endif ()
	endif ()
	add_externaltest (${name})
endforeach (file ${SCRIPT_TESTS})
