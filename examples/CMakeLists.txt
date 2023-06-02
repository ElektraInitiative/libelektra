include (LibAddMacros)
include (LibAddBinding)

# don't call add_headers in a loop
add_headers (HDR_FILES)

macro (do_example source)
	set (SOURCES ${HDR_FILES} ${source}.c)
	add_executable (${source} ${SOURCES})
	target_include_directories (${source} PRIVATE "${CMAKE_SOURCE_DIR}/src/include" "${CMAKE_BINARY_DIR}/src/include")

	target_link_elektra (${source} elektra-kdb)

	set_target_properties (${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_example)

file (GLOB TESTS *.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	if (NOT "${name}" MATCHES "^notification")
		do_example (${name})
	endif ()
endforeach (file ${TESTS})

target_link_elektra (cascading elektra-kdb)
target_link_elektra (cmerge elektra-merge)
target_link_elektra (ksCut elektra-kdb)
target_link_elektra (kdbget elektra-kdb)
target_link_elektra (kdbget_error elektra-kdb)
target_link_elektra (kdbintro elektra-kdb)
target_link_elektra (kdbopen elektra-kdb)
target_link_elektra (kdbset elektra-kdb elektra-merge)
target_link_elektra (set_key elektra-kdb)
target_link_elektra (opts elektra-ease elektra-opts)
target_link_elektra (optsCommands elektra-ease elektra-opts)
target_link_elektra (gopts elektra-ease elektra-kdb)
target_link_elektra (goptsCommands elektra-ease elektra-kdb)
target_link_elektra (changetracking elektra-kdb)

# Notification examples

# Cannot build examples without notification library which requires the internalnotification plugin
list (FIND ADDED_PLUGINS "internalnotification" FOUND_NAME)
if (FOUND_NAME GREATER -1)

	# Build notification polling example
	set (EXAMPLE notificationPolling)

	set (SRC_FILES notificationPolling.c)
	set (SOURCES ${SRC_FILES} ${HDR_FILES})

	add_executable (${EXAMPLE} ${SOURCES})
	target_include_directories (${EXAMPLE} PRIVATE "${CMAKE_SOURCE_DIR}/src/include" "${CMAKE_BINARY_DIR}/src/include")

	target_link_elektra (${EXAMPLE} elektra-kdb elektra-notification)

	check_binding_was_added ("io_uv" IS_INCLUDED)
	if (IS_INCLUDED)

		# Build notification async example
		set (EXAMPLE notificationAsync)

		set (SRC_FILES notificationAsync.c)
		set (SOURCES ${SRC_FILES} ${HDR_FILES})
		if (BUILD_FULL OR BUILD_STATIC)
			list (APPEND SOURCES $<TARGET_OBJECTS:elektra-io-uv-objects>) # add sources for elektra-io-uv for static and full
			# builds
		endif ()

		add_executable (${EXAMPLE} ${SOURCES})
		target_include_directories (${EXAMPLE} PRIVATE "${CMAKE_SOURCE_DIR}/src/include" "${CMAKE_BINARY_DIR}/src/include")

		target_link_elektra (${EXAMPLE} elektra-kdb elektra-notification elektra-io elektra-io-uv)
		if (BUILD_FULL OR BUILD_STATIC)
			target_link_libraries (${EXAMPLE} ${PC_libuv_LDFLAGS})
		endif ()

		find_package (libuv QUIET) # Make sure `libuv_VERSION` is defined correctly
		if (libuv_VERSION VERSION_LESS "1.0")
			target_compile_definitions (${EXAMPLE} PRIVATE "HAVE_LIBUV0")
		else ()
			target_compile_definitions (${EXAMPLE} PRIVATE "HAVE_LIBUV1")
		endif ()
		target_include_directories (${EXAMPLE} PRIVATE ${libuv_INCLUDE_DIRS})

	endif ()

	check_binding_was_added ("io_glib" IS_INCLUDED)
	if (IS_INCLUDED)

		# Build notification reload example
		set (EXAMPLE notificationReload)

		set (SRC_FILES notificationReload.c)
		set (SOURCES ${SRC_FILES} ${HDR_FILES})
		set_source_files_properties (${SRC_FILES} PROPERTIES COMPILE_FLAGS "-Wno-pedantic")

		find_package (GLib QUIET)
		list (APPEND SOURCES $<TARGET_OBJECTS:elektra-io-glib-objects>)
		add_executable (${EXAMPLE} ${SOURCES})
		target_include_directories (${EXAMPLE} PRIVATE ${GLib_INCLUDE_DIRS})
		target_include_directories (${EXAMPLE} PRIVATE "${CMAKE_SOURCE_DIR}/src/include" "${CMAKE_BINARY_DIR}/src/include")

		target_link_elektra (${EXAMPLE} elektra-kdb elektra-notification elektra-io elektra-io-glib)
		if (BUILD_FULL OR BUILD_STATIC)
			target_link_libraries (${EXAMPLE} ${GLib_LIBRARY})
		endif ()

	endif ()

endif ()
