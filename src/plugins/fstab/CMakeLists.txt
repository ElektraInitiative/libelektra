try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/fstab/testmntent.c"
		)

include(LibAddMacros)

if (HAS_MNTENT)
	set (SOURCES fstab.h fstab.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	configure_file (fstab ${CMAKE_CURRENT_BINARY_DIR}/fstab)

	add_library (elektra-fstab MODULE ${SOURCES})
	install (TARGETS elektra-fstab DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	add_plugintest (fstab)
else (HAS_MNTENT)
	remove_plugin (fstab "mntent is missing")
endif (HAS_MNTENT)
