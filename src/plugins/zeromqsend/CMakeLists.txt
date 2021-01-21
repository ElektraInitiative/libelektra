if (DEPENDENCY_PHASE)
	find_package (ZeroMQ QUIET)

	if (NOT ZeroMQ_FOUND)
		remove_plugin (zeromqsend "package libzmq (libzmq3-dev) not found")
	endif ()
endif ()

add_plugin (
	zeromqsend
	SOURCES zeromqsend.h zeromqsend.c publish.c
	INCLUDE_DIRECTORIES ${ZeroMQ_INCLUDE_DIR}
	LINK_LIBRARIES ${ZeroMQ_LIBRARIES} COMPONENT libelektra${SO_VERSION}-zeromq)

if (ADDTESTING_PHASE) # the test requires pthread
	find_package (Threads QUIET)

	if (BUILD_TESTING)
		add_plugintest (zeromqsend TEST_LINK_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})
	endif ()
endif ()
