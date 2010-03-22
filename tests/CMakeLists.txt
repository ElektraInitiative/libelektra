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
##

include (LibAddMacros)

#dont call add_headers in a loop
add_headers (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c tests.c tests.h)
	add_executable (${source} ${SOURCES})
	target_link_libraries (${source} elektra-static elektratools-static)
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

do_test (test_key)
do_test (test_type)
do_test (test_args)
do_test (test_ks)
do_test (test_kdb)

include_directories ("${PROJECT_SOURCE_DIR}/tests")
