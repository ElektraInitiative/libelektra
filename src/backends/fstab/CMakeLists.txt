try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/backends/fstab/testmntent.c")

if (HAS_MNTENT)
	include(LibAddMacros)

	set (SOURCES fstab.h fstab.c)
	add_headers(SOURCES)
	add_library(elektra-fstab MODULE ${SOURCES})
else (HAS_MNTENT)
	message ("Excluded Backend fstab because mntent is missing")
endif (HAS_MNTENT)
