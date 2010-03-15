set (SOURCES fstab.h fstab.c)

try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/backends/fstab/testmntent.c")

if (HAS_MNTENT)
	include_directories("${PROJECT_BINARY_DIR}/src/include")
	add_library(elektra-fstab MODULE ${SOURCES})
else (HAS_MNTENT)
	message ("Excluded Backend fstab because mntent is missing")
endif (HAS_MNTENT)
