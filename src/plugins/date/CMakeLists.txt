include (LibAddMacros)

if (DEPENDENCY_PHASE)

	# The plugin does not work correctly if we compile it with GCC 4.7
	if (CMAKE_CXX_COMPILER_ID MATCHES "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.8)
		remove_plugin (date "gcc version too old (gcc -dumpversion < 4.8)")
	endif (CMAKE_CXX_COMPILER_ID MATCHES "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.8)
endif (DEPENDENCY_PHASE)

add_plugin (
	date
	SOURCES date.h date.c
	ADD_TEST COMPONENT libelektra${SO_VERSION}-extra)
