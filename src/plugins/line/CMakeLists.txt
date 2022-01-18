if (DEPENDENCY_PHASE)
	try_compile (COMPAT_GETLINE "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/line/testgetline.c")

	if (NOT COMPAT_GETLINE)
		remove_plugin (line "Unable to use getline function. Check libc version, should be higher than 4.6.27.")
	endif ()
endif ()

add_plugin (
	line
	SOURCES line.h line.c
	LINK_ELEKTRA elektra-ease
	INSTALL_TEST_DATA TEST_README COMPONENT libelektra${SO_VERSION})

if (ADDTESTING_PHASE)
	add_plugintest (line INSTALL_TEST_DATA)
endif ()
