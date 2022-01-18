set (SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/io.c")

set (LIBRARY_NAME elektra-io)

add_lib (
	io
	SOURCES
	${SOURCES}
	LINK_ELEKTRA
	elektra-kdb
	elektra-invoke
	COMPONENT
	libelektra${SO_VERSION})

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc" @ONLY)

install (
	FILES "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc"
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	COMPONENT libelektra-dev)

add_subdirectory (adapter)
