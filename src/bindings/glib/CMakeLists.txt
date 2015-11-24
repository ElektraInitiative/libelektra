include (LibAddMacros)

find_package (PkgConfig)
pkg_check_modules (GOBJECT gobject-2.0>=2.36)

if (NOT GOBJECT_FOUND)
	set (REGEX "gi_.*")
	list_filter(BINDINGS REGEX)
	remove_binding (glib "No gobject found using pkg-config, also remove gi bindings because they depend on it")
else()
	message(STATUS "Include Binding glib")

	file (GLOB GELEKTRA_HDR_FILES *.h)
	file (GLOB GELEKTRA_SRC_FILES *.c)

	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${GELEKTRA_SRC_FILES} ${GELEKTRA_HDR_FILES} ${ELEKTRA_HEADERS})

	include_directories (${CMAKE_CURRENT_SOURCE_DIR})

	add_definitions (${GOBJECT_CFLAGS})

	add_library (${GELEKTRA_LIBRARY} SHARED ${SOURCES})

	target_link_libraries (${GELEKTRA_LIBRARY} elektra-core elektra-kdb)
	target_link_libraries (${GELEKTRA_LIBRARY} ${GOBJECT_LIBRARIES})

	install (
		TARGETS ${GELEKTRA_LIBRARY}
		LIBRARY DESTINATION lib${LIB_SUFFIX}
	)

	install (
		FILES ${GELEKTRA_HDR_FILES}
		DESTINATION include/${TARGET_INCLUDE_FOLDER}
	)

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/gelektra-constants.h.in"
		"${CMAKE_CURRENT_BINARY_DIR}/gelektra-constants.h"
		@ONLY
	)

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/gelektra.pc.in"
		"${CMAKE_CURRENT_BINARY_DIR}/${GELEKTRA_LIBRARY}.pc"
		@ONLY
	)

	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/${GELEKTRA_LIBRARY}.pc"
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	)

	if (BUILD_TESTING)
		add_subdirectory (tests)
	endif (BUILD_TESTING)
endif()
