include (LibAddMacros)

find_package (PkgConfig QUIET)
pkg_check_modules (GOBJECT QUIET gobject-2.0>=2.36)

# Test if binding can be build
if (NOT GOBJECT_FOUND)
	exclude_binding (glib "No gobject found using pkg-config, also remove gi bindings because they depend on it")
	return ()
endif (NOT GOBJECT_FOUND)

if (ENABLE_ASAN)
	exclude_binding (glib "glib is not compatible with ENABLE_ASAN")
	return ()
endif (ENABLE_ASAN)

if (NOT BUILD_SHARED)
	exclude_binding (glib "it can only be built if `BUILD_SHARED` is enabled")
	return ()
endif (NOT BUILD_SHARED)

# Add binding
add_binding (glib)

set (
	GELEKTRA_LIBRARY
	gelektra-${SO_VERSION}.0
	CACHE INTERNAL "GELEKTRA_LIBRARY")

file (GLOB GELEKTRA_HDR_FILES *.h)
file (GLOB GELEKTRA_SRC_FILES *.c)
set (SOURCES ${GELEKTRA_SRC_FILES} ${GELEKTRA_HDR_FILES} ${ELEKTRA_HEADERS})

add_library (${GELEKTRA_LIBRARY} SHARED ${SOURCES})

target_include_directories (${GELEKTRA_LIBRARY} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (${GELEKTRA_LIBRARY} SYSTEM PUBLIC ${GOBJECT_INCLUDE_DIRS})

target_compile_options (${GELEKTRA_LIBRARY} PUBLIC ${GOBJECT_CFLAGS_OTHER} "-Wno-pedantic")

target_link_libraries (${GELEKTRA_LIBRARY} PUBLIC elektra-core elektra-kdb)
target_link_libraries (${GELEKTRA_LIBRARY} PUBLIC ${GOBJECT_LDFLAGS})

install (TARGETS ${GELEKTRA_LIBRARY} LIBRARY DESTINATION lib${LIB_SUFFIX} COMPONENT glib-elektra)

install (
	FILES ${GELEKTRA_HDR_FILES}
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	COMPONENT glib-elektra)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/gelektra-constants.h.in" "${CMAKE_CURRENT_BINARY_DIR}/gelektra-constants.h" @ONLY)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/gelektra.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${GELEKTRA_LIBRARY}.pc" @ONLY)

install (
	FILES "${CMAKE_CURRENT_BINARY_DIR}/${GELEKTRA_LIBRARY}.pc"
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	COMPONENT glib-elektra)

if (BUILD_TESTING)
	add_subdirectory (tests)
endif (BUILD_TESTING)
