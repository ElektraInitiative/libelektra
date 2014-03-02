include (LibAddMacros)

configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/include_common.sh.in"
		"${CMAKE_CURRENT_BINARY_DIR}/include_common.sh"
		@ONLY
	)

file(READ "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh"
	INCLUDE_COMMON_FILE)

# Add a test for a script
#
# The given testname is blah.sh
#   the script file must be called blah.sh
#   it will be installed on the system as blah.sh
#   the test will be called testscr_blah
#   and the script file for the test will be testscr_blah.sh
function (add_scripttest testname)
	get_filename_component (testname_we ${testname} NAME_WE)

	set (filename ${CMAKE_CURRENT_SOURCE_DIR}/${testname})
	if (NOT EXISTS ${filename})
		message (SEND_ERROR "add_scripttest: given file ${filename} does not exists")
	endif (NOT EXISTS ${filename})

	foreach (TARGET true false)
		if (TARGET)
			if (BUILD_FULL)
				set (KDB_COMMAND "kdb-full")
			else (BUILD_FULL)
				if (BUILD_STATIC)
					set (KDB_COMMAND "kdb-static")
				else (BUILD_STATIC)
					set (KDB_COMMAND "kdb")
				endif (BUILD_STATIC)
			endif (BUILD_FULL)
		else (TARGET)
			if (BUILD_FULL)
				set (KDB_COMMAND "${PROJECT_BINARY_DIR}/src/tools/kdb/kdb-full")
			else (BUILD_FULL)
				if (BUILD_STATIC)
					set (KDB_COMMAND "${PROJECT_BINARY_DIR}/src/tools/kdb/kdb-static")
				else (BUILD_STATIC)
					message (WARNING "add_scripttest: neither full nor static activated, cant run ${testname}")
				endif (BUILD_STATIC)
			endif (BUILD_FULL)
		endif (TARGET)

		set(INCLUDE_COMMON
			"${INCLUDE_COMMON_FILE}KDB=${KDB_COMMAND}"
			)

		if (TARGET)
			configure_file(
				"${filename}"
				"${CMAKE_CURRENT_BINARY_DIR}/${testname}"
				@ONLY
				)
			install (FILES "${CMAKE_CURRENT_BINARY_DIR}/${testname}"
				DESTINATION bin
				PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
				GROUP_READ GROUP_EXECUTE
				WORLD_READ WORLD_EXECUTE
				)
		else()
			set(testscriptname
				"${CMAKE_CURRENT_BINARY_DIR}/testscr_${testname}")
			configure_file(
				"${filename}"
				"${testscriptname}"
				@ONLY
				 )
			add_test (
				testscr_${testname_we}
				"${testscriptname}"
				)
		endif()
	endforeach()

endfunction (add_scripttest)


file (COPY data
	DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

file (GLOB SCRIPT_TESTS *.sh)
foreach (file ${SCRIPT_TESTS})
	get_filename_component (name ${file} NAME)
	add_scripttest(${name})
endforeach (file ${SCRIPT_TESTS})
