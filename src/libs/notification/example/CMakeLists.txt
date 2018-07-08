include (LibAddMacros)
include (LibAddBinding)

# Build example_notification
set (EXAMPLE example_notification)

set (SRC_FILES example_notification.c)
add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${ELEKTRA_HEADERS})

add_executable (${EXAMPLE} ${SOURCES})
add_dependencies (${EXAMPLE} kdberrors_generated)

# target_include_directories (${EXAMPLE} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

target_link_elektra (${EXAMPLE} elektra-kdb elektra-notification)

# - The asynchronous example requires io_uv
# - io_uv is disabled for ASAN builds, see https://github.com/ElektraInitiative/libelektra/issues/2007
check_binding_included ("io_uv" IS_INCLUDED SUBDIRECTORY "io/uv" SILENT)
find_package (PkgConfig)
pkg_check_modules (LIBUV QUIET libuv)
if (IS_INCLUDED AND LIBUV_FOUND AND NOT ENABLE_ASAN)

	set (EXAMPLE example_notification_async) # Build example_notification_async

	set (SRC_FILES example_notification_async.c)
	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${SRC_FILES} ${ELEKTRA_HEADERS})
	if (BUILD_FULL OR BUILD_STATIC)

		list (APPEND SOURCES
			     $<TARGET_OBJECTS:OBJ_elektra-io-uv>) # add sources for elektra-io-uv for static and full builds
	endif ()

	add_executable (${EXAMPLE} ${SOURCES})
	add_dependencies (${EXAMPLE} kdberrors_generated)

	target_link_elektra (${EXAMPLE} elektra-kdb elektra-notification elektra-io elektra-io-uv)
	if (BUILD_FULL OR BUILD_STATIC)
		target_link_libraries (${EXAMPLE} ${LIBUV_LDFLAGS})
	endif ()

	if (LIBUV_VERSION VERSION_LESS "1.0")
		target_compile_definitions (${EXAMPLE} PRIVATE "HAVE_LIBUV0")
	else ()
		target_compile_definitions (${EXAMPLE} PRIVATE "HAVE_LIBUV1")
	endif ()
endif ()
