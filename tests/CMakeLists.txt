## Elektra Test Suite
#
# This file is responsible for generating all tests
# regarding elektra's core.
#
# Backends and Bindings specific tests might be in
# their folders.
#
# To run all tests, simply type:
#  make test
#
# To run specific tests, type:
#  ctest -R name
#
# To run tests with valgrind, type:
#  ctest -T memcheck
#
# To run verbose (to see output and commands), type:
#  ctest -V
#
# For example, to run keyset test with valgrind and verbose, type:
#  ctest -V -T memcheck -R ks
#
##

include (LibAddMacros)

#don't call add_headers in a loop
add_headers (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c tests.c)
	add_executable (${source} ${SOURCES})

	if (INSTALL_TESTING)
		install (TARGETS ${source}
			DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
	endif (INSTALL_TESTING)

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full)
	else (BUILD_FULL)
		target_link_libraries (${source} elektra-static)
	endif (BUILD_FULL)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
	set_target_properties (${source} PROPERTIES LINKER_LANGUAGE CXX)
	add_test (${source}
			"${PROJECT_BINARY_DIR}/tests/${source}"
			"${PROJECT_BINARY_DIR}/tests"
			)
endmacro (do_test)

file (GLOB TESTS test_*.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
endforeach (file ${TESTS})

add_subdirectory (shell)

include_directories ("${PROJECT_SOURCE_DIR}/tests")
