try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/backends/fstab/testmntent.c")

include(LibAddMacros)

if (HAS_MNTENT)
	set (SOURCES fstab.h fstab.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	add_library (elektra-fstab MODULE ${SOURCES})
	install (TARGETS elektra-fstab DESTINATION lib/elektra)

	add_plugintest (fstab)
else (HAS_MNTENT)
	remove_backend (fstab "mntent is missing")
endif (HAS_MNTENT)
