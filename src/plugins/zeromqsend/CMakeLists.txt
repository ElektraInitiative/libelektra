include (LibAddBinding)

if (DEPENDENCY_PHASE)
	find_package (PkgConfig)
	pkg_check_modules (ZMQ QUIET libzmq)

	if (NOT ZMQ_FOUND)
		remove_plugin (zeromqsend "libzmq package not found")
	endif ()
endif ()

add_plugin (zeromqsend
	SOURCES
		zeromqsend.h zeromqsend.c
		publish.c
	INCLUDE_DIRECTORIES
		${ZMQ_INCLUDE_DIR}
	LINK_ELEKTRA
		elektra-io-adapter-zeromq
	LINK_LIBRARIES
		${ZMQ_LIBRARIES}
	)

if (ADDTESTING_PHASE)
	# the test requires an IO binding; using io_uv for now
	find_package (Threads)

	check_binding_was_added ("io_uv" IS_INCLUDED)
	if (BUILD_TESTING AND IS_INCLUDED)
		if (BUILD_FULL OR BUILD_STATIC)
			find_package (PkgConfig)
			pkg_check_modules (LIBUV QUIET libuv>=1)
			# libuv is present; otherwise io_uv bindings would not be added
			set (STATIC_LIBRARIES ${LIBUV_LIBRARIES})
		endif ()

		add_plugintest (zeromqsend
			LINK_LIBRARIES
				${ZMQ_LIBRARIES}
				${STATIC_LIBRARIES}
				${CMAKE_THREAD_LIBS_INIT}
			INCLUDE_DIRECTORIES
				${ZMQ_INCLUDE_DIR}
			LINK_ELEKTRA
				elektra-io-adapter-zeromq
				elektra-io-uv
			)

		# add sources manually because add_plugintest does not support generator
		# expressions for additional source files
		if (BUILD_FULL OR BUILD_STATIC)
			# add sources for elektra-io-uv for static and full builds
			target_sources (testmod_zeromqsend PRIVATE $<TARGET_OBJECTS:OBJ_elektra-io-uv>)
		endif ()
	else ()
		message (WARNING "io_uv bindings are required for testing, test deactivated")
	endif ()
endif ()
