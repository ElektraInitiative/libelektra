enable_testing()

macro (do_test source)
	add_executable (${source} ${source}.c tests.c)
	target_link_libraries (${source} elektra)
	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H)
	add_test (test_key "${PROJECT_BINARY_DIR}/tests/${source}")
endmacro (do_test)

do_test (test_key)
do_test (test_type)

include_directories("${PROJECT_BINARY_DIR}/src/include")
include_directories("${PROJECT_SOURCE_DIR}/tests")

