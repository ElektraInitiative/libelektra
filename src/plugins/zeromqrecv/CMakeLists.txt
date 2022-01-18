include (LibAddBinding)

if (DEPENDENCY_PHASE)
	find_package (ZeroMQ QUIET)

	if (NOT ZeroMQ_FOUND)
		remove_plugin (zeromqrecv "package libzmq (libzmq3-dev) not found")
	endif ()
endif ()

add_plugin (
	zeromqrecv
	SOURCES zeromqrecv.h zeromqrecv.c subscribe.c
	OBJECT_SOURCES $<TARGET_OBJECTS:io-adapter-zeromq>
	INCLUDE_DIRECTORIES ${ZeroMQ_INCLUDE_DIR}
	LINK_ELEKTRA elektra-io
	LINK_LIBRARIES ${ZeroMQ_LIBRARIES} COMPONENT libelektra${SO_VERSION}-zeromq)

if (ADDTESTING_PHASE)
	check_binding_was_added ("io_uv" IS_INCLUDED)
	if (BUILD_TESTING AND IS_INCLUDED)
		find_package (libuv QUIET) # libuv is present; otherwise io_uv bindings would not be added

		if (BUILD_FULL OR BUILD_STATIC)
			set (STATIC_LIBRARIES ${libuv_LIBRARIES})
		endif ()

		add_plugintest (
			zeromqrecv
			TEST_LINK_LIBRARIES ${STATIC_LIBRARIES}
			TEST_LINK_ELEKTRA elektra-io-uv
			INCLUDE_SYSTEM_DIRECTORIES ${libuv_INCLUDE_DIRS})
		if (libuv_VERSION VERSION_LESS "1.0")
			target_compile_definitions (testmod_zeromqrecv PRIVATE "HAVE_LIBUV0")
		else ()
			target_compile_definitions (testmod_zeromqrecv PRIVATE "HAVE_LIBUV1")
		endif ()

		# add sources manually because add_plugintest does not support generator expressions for additional source files
		if (BUILD_FULL OR BUILD_STATIC) # add sources for elektra-io-uv for static and full builds
			target_sources (testmod_zeromqrecv PRIVATE $<TARGET_OBJECTS:elektra-io-uv-objects>)
		endif ()
	else ()
		message (WARNING "io_uv bindings are required for testing, test deactivated")
	endif ()
endif ()
