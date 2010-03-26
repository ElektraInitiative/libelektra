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

#dont call add_headers in a loop
add_headers (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c tests.c tests.h)
	add_executable (${source} ${SOURCES})

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full elektratools)
	else (BUILD_FULL)
		target_link_libraries (${source} elektra-static elektratools-static elektra-static)
	endif (BUILD_FULL)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H)
	add_test (${source}
			"${PROJECT_BINARY_DIR}/tests/${source}"
			"${PROJECT_BINARY_DIR}/tests"
			)
endmacro (do_test)

file (GLOB XML_FILES *.xml)
foreach (file ${XML_FILES})
	copy_file (${file} "${CMAKE_CURRENT_BINARY_DIR}")
endforeach (file ${XML_FILES})

file (GLOB TESTS test_*.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
endforeach (file ${TESTS})

if (BUILD_FULL)
	get_target_property (KDB_COMMAND kdb-full LOCATION)
else (BUILD_FULL)
	get_target_property (KDB_COMMAND kdb-static LOCATION)
endif (BUILD_FULL)


configure_file(
		test_script.sh.in
		"${PROJECT_BINARY_DIR}/tests/test_script.sh"
	      )
add_test (test_script "${PROJECT_BINARY_DIR}/tests/test_script.sh" "${PROJECT_BINARY_DIR}/tests")

include_directories ("${PROJECT_SOURCE_DIR}/tests")
