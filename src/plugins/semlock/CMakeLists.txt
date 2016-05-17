find_package(Threads)
include (LibAddMacros)

add_plugin (semlock
	SOURCES
		semlock.h
		semlock.c
	LINK_LIBRARIES
		${CMAKE_THREAD_LIBS_INIT}
	ADD_TEST
	)
