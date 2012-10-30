try_compile (HAS_UNAME
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/uname/testuname.c"
		)

include(LibAddMacros)

if (HAS_MNTENT)
	set (SOURCES uname.h uname.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	add_library (elektra-uname MODULE ${SOURCES})
	install (TARGETS elektra-uname DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	add_plugintest (uname)
else (HAS_MNTENT)
	remove_plugin (uname "uname is missing")
endif (HAS_MNTENT)
