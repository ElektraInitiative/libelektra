find_package (Threads QUIET)
include (LibAddMacros)

add_plugin (semlock
	    # ADD_TEST
	    SOURCES semlock.h
		    semlock.c
	    LINK_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})
