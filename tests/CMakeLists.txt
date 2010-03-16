include (LibAddMacros)
#dont call add_headers in a loop
add_headers (HDR_FILES)

macro (do_test source)
	set (SOURCES ${HDR_FILES} ${source}.c tests.c tests.h)
	add_executable (${source} ${SOURCES})
	target_link_libraries (${source} elektra)
	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H)
	add_test (${source} "${PROJECT_BINARY_DIR}/tests/${source}")
endmacro (do_test)


do_test (test_key)
do_test (test_type)

include_directories ("${PROJECT_SOURCE_DIR}/tests")
