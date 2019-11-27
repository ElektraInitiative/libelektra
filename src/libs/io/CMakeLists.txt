set (SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/io.c")

set (LIBRARY_NAME elektra-io)

add_lib (io SOURCES ${SOURCES} LINK_ELEKTRA elektra-kdb elektra-invoke)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc" @ONLY)

install (FILES "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc" DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER})

add_subdirectory (adapter)
