## Elektra Test Suite
#
# This file is responsible for generating all tests
# regarding elektra's core.
#
# Backends and Bindings specific tests might be in
# their folders.

include (LibAddMacros)

#don't call add_headers in a loop
add_headers (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c $<TARGET_OBJECTS:cframework>)
	add_headers(SOURCES)
	add_testheaders(SOURCES)
	add_executable (${source} ${SOURCES})
	add_dependencies (${source} kdberrors_generated)

	if (INSTALL_TESTING)
		install (TARGETS ${source}
			DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
	endif (INSTALL_TESTING)

	target_link_elektra(${source})

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
	set_target_properties (${source} PROPERTIES LINKER_LANGUAGE CXX)
	add_test (${source}
			"${CMAKE_BINARY_DIR}/bin/${source}"
			"${CMAKE_CURRENT_BINARY_DIR}"
			)
endmacro (do_test)

add_custom_target(run_all
	COMMAND ${CMAKE_CTEST_COMMAND}
	--force-new-ctest-process --output-on-failure
	--build-config "$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_custom_target(run_memcheck
	COMMAND "${CMAKE_SOURCE_DIR}/scripts/run_memcheck"
	"$<CONFIGURATION>"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

include_directories(data)

add_subdirectory (cframework)
add_subdirectory (shell)
add_subdirectory (shell/shell_recorder)
add_subdirectory (abi)
add_subdirectory (ctest)
if (ENABLE_KDB_TESTING)
	add_subdirectory (kdb)
endif (ENABLE_KDB_TESTING)
