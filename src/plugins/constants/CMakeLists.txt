include (LibAddMacros)

if (DEPENDENCY_PHASE)
	configure_file (constants.c.in ${CMAKE_BINARY_DIR}/constants.c)
endif ()

add_plugin (constants
	SOURCES
		${CMAKE_BINARY_DIR}/constants.c
	)
