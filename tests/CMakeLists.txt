add_executable (test_key test_key.c tests.c)
target_link_libraries (test_key elektra)
set_target_properties (test_key PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H)

include_directories("${PROJECT_BINARY_DIR}/src/include")
include_directories("${PROJECT_SOURCE_DIR}/tests")

add_test (test_key ${EXECUTEABLE_OUTPUT_PATH}/test_key)
