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

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full elektratools)
	else (BUILD_FULL)
		if (BUILD_STATIC)
			target_link_libraries (${source} elektra-static elektratools-static elektra-static)
		else (BUILD_STATIC)
			message (FATAL_ERROR "Test cases should not be included if neither BUILD_FULL nor BUILD_STATIC is ON")
		endif (BUILD_STATIC)
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

include_directories ("${PROJECT_SOURCE_DIR}/tests")
