if (DEPENDENCY_PHASE)
	try_compile (HAS_MNTENT
			"${CMAKE_BINARY_DIR}"
			"${PROJECT_SOURCE_DIR}/src/plugins/fstab/testmntent.c"
			)

	if (NOT HAS_MNTENT)
		remove_plugin (fstab "function mntent is missing")
	endif ()
endif ()

add_plugin (fstab
	SOURCES
		fstab.c
		fstab.h
	LINK_ELEKTRA
		elektra-meta
	ADD_TEST
	INSTALL_TEST_DATA
	)
