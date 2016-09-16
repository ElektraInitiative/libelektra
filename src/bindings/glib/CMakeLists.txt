include (LibAddMacros)

if (CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION EQUAL 8)
	if (CMAKE_PATCH_VERSION LESS 12)
		# needed for PRIVATE and PUBLIC flags in target_link_libraries
		# also in tests subdirectory
		# see also https://cmake.org/cmake/help/v3.0/policy/CMP0023.html
		remove_binding(jna "cmake 2.8.12 needed for glib")
		return()
	endif()
endif()

find_package (PkgConfig)
pkg_check_modules (GOBJECT gobject-2.0>=2.36)

if (NOT GOBJECT_FOUND)
	set (REGEX "gi_.*")
	list_filter(BINDINGS REGEX)
	remove_binding (glib "No gobject found using pkg-config, also remove gi bindings because they depend on it")
	remove_binding (gsettings "Because they depend on glib.")
else()
	message(STATUS "Include Binding glib")

	file (GLOB GELEKTRA_HDR_FILES *.h)
	file (GLOB GELEKTRA_SRC_FILES *.c)
	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${GELEKTRA_SRC_FILES} ${GELEKTRA_HDR_FILES} ${ELEKTRA_HEADERS})

	add_library (${GELEKTRA_LIBRARY} SHARED ${SOURCES})
	add_dependencies (${GELEKTRA_LIBRARY} kdberrors_generated)

	target_include_directories (${GELEKTRA_LIBRARY} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
	target_include_directories (${GELEKTRA_LIBRARY} SYSTEM PUBLIC ${GOBJECT_INCLUDE_DIRS})

	target_compile_options (${GELEKTRA_LIBRARY} PUBLIC ${GOBJECT_CFLAGS_OTHER})

	target_link_libraries (${GELEKTRA_LIBRARY} PUBLIC elektra-core elektra-kdb)
	target_link_libraries (${GELEKTRA_LIBRARY} PUBLIC ${GOBJECT_LDFLAGS})

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
