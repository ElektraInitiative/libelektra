file (GLOB SOURCES
	   *.c)

set_source_files_properties (${PROJECT_BINARY_DIR}/src/error/elektra_errors.c PROPERTIES GENERATED ON SKIP_AUTOMOC ON)
add_lib (highlevel SOURCES ${SOURCES} ${PROJECT_BINARY_DIR}/src/error/elektra_errors.c LINK_ELEKTRA elektra-core elektra-kdb elektra-ease)
add_dependencies (elektra-highlevel elektra_error_codes_generated)
