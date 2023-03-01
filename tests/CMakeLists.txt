# ~~~
# Elektra Test Suite
#
# This file is responsible for generating all tests
# regarding Elektra's core.
#
# Backends and Bindings specific tests might be in
# their folders.
# ~~~

include (LibAddMacros)

if (ENABLE_TESTING)

	include (FindGTest)

	if (DEFINED GOOGLETEST_ROOT)
		set (googletest_SOURCE_DIR "${GOOGLETEST_ROOT}")
		set (googletest_BINARY_DIR "${CMAKE_BINARY_DIR}/gtest")
	else (DEFINED GOOGLETEST_ROOT)
		include (DownloadProject)

		download_project (
			PROJ googletest
			GIT_REPOSITORY https://github.com/google/googletest.git
			GIT_TAG v1.13.0 QUIET UPDATE_DISCONNECTED 1)
	endif (DEFINED GOOGLETEST_ROOT)

	# Prevent GoogleTest from overriding our compiler/linker options when building with Visual Studio
	set (
		gtest_force_shared_crt
		ON
		CACHE BOOL "" FORCE)

	add_subdirectory (${googletest_SOURCE_DIR} ${googletest_BINARY_DIR} EXCLUDE_FROM_ALL)

	set_property (TARGET gtest PROPERTY COMPILE_FLAGS "-Wno-undef")
	set_property (TARGET gtest_main PROPERTY COMPILE_FLAGS "-Wno-undef")
endif (ENABLE_TESTING)

# don't call add_headers in a loop
add_headers (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c $<TARGET_OBJECTS:cframework>)
	add_headers (SOURCES)
	add_testheaders (SOURCES)
	add_executable (${source} ${SOURCES})

	if (INSTALL_TESTING)
		install (
			TARGETS ${source}
			DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
			COMPONENT elektra-tests)
	endif (INSTALL_TESTING)

	target_link_elektra (${source})

	set_target_properties (${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
	set_target_properties (${source} PROPERTIES LINKER_LANGUAGE CXX)
	add_test (${source} "${CMAKE_BINARY_DIR}/bin/${source}" "${CMAKE_CURRENT_BINARY_DIR}")
	set_property (TEST ${source} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
endmacro (do_test)

# Since we want to see the output of the tests immediately we specify the parameter `USES_TERMINAL` in the targets below. If we do not do
# that, then the output of the commands will be buffered.
add_custom_target (
	run_all
	COMMAND "${CMAKE_BINARY_DIR}/scripts/dev/run_all" "$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
	USES_TERMINAL)

add_custom_target (
	run_checkshell
	COMMAND "${CMAKE_BINARY_DIR}/scripts/dev/run_checkshell" "$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
	USES_TERMINAL)

add_custom_target (
	run_nocheckshell
	COMMAND "${CMAKE_BINARY_DIR}/scripts/dev/run_nocheckshell" "$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
	USES_TERMINAL)

add_custom_target (
	run_nokdbtests
	COMMAND "${CMAKE_BINARY_DIR}/scripts/dev/run_nokdbtests" "$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_custom_target (
	run_memcheck
	COMMAND "${CMAKE_BINARY_DIR}/scripts/dev/run_memcheck" "$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

include_directories (data)

add_subdirectory (cframework)
add_subdirectory (shell)
add_subdirectory (abi)
add_subdirectory (ctest)
if (ENABLE_KDB_TESTING)
	add_subdirectory (kdb)
endif (ENABLE_KDB_TESTING)
