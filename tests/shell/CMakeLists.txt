include (LibAddMacros)

get_property (SHARED_ONLY_PLUGINS GLOBAL PROPERTY SHARED_ONLY_PLUGINS)
set (ADDED_PLUGINS_WITHOUT_ONLY_SHARED ${ADDED_PLUGINS})
if (SHARED_ONLY_PLUGINS)
	list (REMOVE_ITEM ADDED_PLUGINS_WITHOUT_ONLY_SHARED ${SHARED_ONLY_PLUGINS})
endif (SHARED_ONLY_PLUGINS)

# We prefer `kdb` over `kdb-full` over `kdb-static`. Some tests, such as the Markdown Shell Recorder test for the `typechecker` plugin
# (which only supports shared builds), will fail otherwise.
if (BUILD_SHARED)
	set (KDB_COMMAND_BASENAME kdb)
elseif (BUILD_FULL)
	set (KDB_COMMAND_BASENAME kdb-full)
elseif (BUILD_STATIC)
	set (KDB_COMMAND_BASENAME kdb-static)
elseif (ENABLE_KDB_TESTING)
	message (SEND_ERROR "no kdb tool found, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
endif (BUILD_SHARED)

set (IS_INSTALLED "YES")
set (KDB_COMMAND "${KDB_COMMAND_BASENAME}")
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/include_common.sh.in" "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh" @ONLY)

file (READ "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh" INCLUDE_COMMON_INSTALLED_FILE)

set (IS_INSTALLED "NO")
set (KDB_COMMAND "${CMAKE_BINARY_DIR}/bin/${KDB_COMMAND_BASENAME}")
configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/include_common.sh.in" "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh" @ONLY)

file (READ "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh" INCLUDE_COMMON_FILE)
file (REMOVE "${CMAKE_CURRENT_BINARY_DIR}/include_common.sh")

# ~~~
# Add a test for a script
#
# The given testname is blah.sh
#   the script file must be called blah.sh
#   it will be installed on the system as blah.sh
#   the test will be called testscr_blah
#   and the script file for the test will be testscr_blah.sh
# ~~~
function (add_scripttest testname)
	get_filename_component (testname_we ${testname} NAME_WE)

	set (filename ${CMAKE_CURRENT_SOURCE_DIR}/${testname})
	if (NOT EXISTS ${filename})
		message (SEND_ERROR "add_scripttest: given file ${filename} does not exists")
	endif (NOT EXISTS ${filename})

	set (COMPONENT_BIN_EXTRA_FILES generate_data)

	foreach (TARGET true false)
		if (TARGET)
			if (INSTALL_TESTING)

				set (
					excluded_tests
					check_bashisms
					check_doc
					check_posix
					check_formatting
					check_gen
					check_oclint
					check_plugins
					check_spelling)

				if ("${testname_we}" IN_LIST COMPONENT_BIN_EXTRA_FILES)
					set (HAS_COMPONENT elektra-bin-extra)
				else ()
					set (HAS_COMPONENT elektra-tests)
				endif ()

				list (FIND excluded_tests "${testname_we}" index_excluded)
				if (index_excluded EQUAL -1)
					set (RACE_COMMAND "$KDB race")
					set (INCLUDE_COMMON "${INCLUDE_COMMON_INSTALLED_FILE}")
					configure_file ("${filename}" "${CMAKE_CURRENT_BINARY_DIR}/${testname}" @ONLY)
					# TODO: make clear via repo structure that this happens
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
						RENAME ${testname_we}
						COMPONENT ${HAS_COMPONENT})
				endif (index_excluded EQUAL -1)

			endif (INSTALL_TESTING)
		else (TARGET)
			if (ENABLE_KDB_TESTING)
				if (NOT ${testname_we} STREQUAL "run_all")
					set (RACE_COMMAND "${CMAKE_BINARY_DIR}/bin/race")
					set (INCLUDE_COMMON "${INCLUDE_COMMON_FILE}")
					set (testscriptname "${CMAKE_CURRENT_BINARY_DIR}/testscr_${testname}")
					configure_file ("${filename}" "${testscriptname}" @ONLY)
					add_test (testscr_${testname_we} "${testscriptname}") # dash does memleak:
					set_property (TEST testscr_${testname_we} PROPERTY LABELS memleak kdbtests)
					set_property (TEST testscr_${testname_we} PROPERTY ENVIRONMENT
											   "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

					if (${testname_we} STREQUAL "check_kdb_internal_suite")
						# the internal_suite test can take extremely long:
						# https://github.com/ElektraInitiative/libelektra/issues/3510
						set_tests_properties (testscr_${testname_we} PROPERTIES TIMEOUT 2400)
					endif (${testname_we} STREQUAL "check_kdb_internal_suite")

					set (parallel_tests check_bashisms check_doc check_oclint check_plugins check_posix)
					list (FIND parallel_tests "${testname_we}" index_parallel)
					if (index_parallel EQUAL -1)
						set_property (TEST testscr_${testname_we} PROPERTY RUN_SERIAL TRUE)
					endif (index_parallel EQUAL -1)

				endif (NOT ${testname_we} STREQUAL "run_all")
			endif (ENABLE_KDB_TESTING)
		endif (TARGET)
	endforeach ()

endfunction (add_scripttest)

if (INSTALL_TESTING)
	# TODO: make clear via repo structure that this happens
	install (
		DIRECTORY "shell"
		DESTINATION ${TARGET_TEST_DATA_FOLDER}
		COMPONENT elektra-tests)
endif (INSTALL_TESTING)

file (GLOB SCRIPT_TESTS *.sh)
foreach (file ${SCRIPT_TESTS})
	get_filename_component (name ${file} NAME)
	if (NOT ENABLE_ASAN OR NOT name MATCHES "check_gen.sh")
		add_scripttest (${name})
	endif ()
endforeach (file ${SCRIPT_TESTS})

add_subdirectory (gen)
add_subdirectory (external)
add_subdirectory (shell_recorder)
