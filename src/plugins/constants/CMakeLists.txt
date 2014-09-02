include (LibAddMacros)


configure_file(constants.c.in ${CMAKE_BINARY_DIR}/constants.c)

add_plugin(constants
	SOURCES
		${CMAKE_BINARY_DIR}/constants.c
	)
