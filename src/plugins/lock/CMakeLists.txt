find_package(Threads)
include (LibAddMacros)

add_plugin (lock
	SOURCES
		lock.h
		lock.c
	LINK_LIBRARIES
		${CMAKE_THREAD_LIBS_INIT}
	)

add_plugintest (lock)
