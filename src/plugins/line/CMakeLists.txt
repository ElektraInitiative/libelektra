if (DEPENDENCY_PHASE)
	try_compile (COMPAT_GETLINE
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/line/testgetline.c"
		)

	if (COMPAT_GETLINE)
		install(DIRECTORY line DESTINATION ${TARGET_TEST_DATA_FOLDER})

		add_plugintest (line)
	else (COMPAT_GETLINE)
		remove_plugin (line "Unable to use getline function. Check libc version, should be higher than 4.6.27.")

	endif (COMPAT_GETLINE)
endif ()

add_plugin (line
	SOURCES
		line.h
		line.c
	LINK_ELEKTRA
		elektra-ease
	)
