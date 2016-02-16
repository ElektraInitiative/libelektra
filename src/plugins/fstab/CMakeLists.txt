try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/fstab/testmntent.c"
		)

include(LibAddMacros)

if (HAS_MNTENT)
	add_plugin(fstab
		SOURCES
			fstab.c
			fstab.h
		LINK_LIBRARIES
			elektra-meta
		)

	install(DIRECTORY fstab DESTINATION ${TARGET_TEST_DATA_FOLDER})

	add_plugintest (fstab)
else (HAS_MNTENT)
	remove_plugin (fstab "mntent is missing")
endif (HAS_MNTENT)
