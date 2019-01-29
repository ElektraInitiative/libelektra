file (GLOB SOURCES
	   *.c)

set_source_files_properties (${PROJECT_BINARY_DIR}/src/error/elektra_errors.c PROPERTIES GENERATED ON)
add_lib (highlevel SOURCES ${SOURCES} ${PROJECT_BINARY_DIR}/src/error/elektra_errors.c LINK_ELEKTRA elektra-kdb elektra-ease)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-highlevel.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra-highlevel.pc" @ONLY)
install (FILES "${CMAKE_CURRENT_BINARY_DIR}/elektra-highlevel.pc" DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER})
