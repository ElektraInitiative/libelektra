file (GLOB SOURCES
	   *.c)

set_source_files_properties (${PROJECT_BINARY_DIR}/src/error/elektra_errors.c PROPERTIES GENERATED ON)
add_lib (highlevel SOURCES ${SOURCES} ${PROJECT_BINARY_DIR}/src/error/elektra_errors.c LINK_ELEKTRA elektra-kdb elektra-ease)
