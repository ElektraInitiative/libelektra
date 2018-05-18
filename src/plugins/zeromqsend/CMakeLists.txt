if (DEPENDENCY_PHASE)
	find_package (PkgConfig)
	pkg_check_modules (ZMQ QUIET libzmq)

	if (NOT ZMQ_FOUND)
		remove_plugin (zeromqsend "libzmq package not found")
	endif ()
endif ()

add_plugin (zeromqsend SOURCES zeromqsend.h zeromqsend.c publish.c INCLUDE_DIRECTORIES ${ZMQ_INCLUDE_DIR} LINK_LIBRARIES ${ZMQ_LIBRARIES})

if (ADDTESTING_PHASE) # the test requires pthread
	find_package (Threads)

	if (BUILD_TESTING)
		add_plugintest (zeromqsend TEST_LINK_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})
	endif ()
endif ()
