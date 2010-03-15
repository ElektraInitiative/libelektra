set (SOURCES fstab.h fstab.c)

try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/backends/fstab/testmntent.c")

set (HAS_MNTENT 1)
if (HAS_MNTENT)
	include_directories("${PROJECT_BINARY_DIR}/src/include")
	add_library(elektra-fstab MODULE ${SOURCES})
endif (HAS_MNTENT)
