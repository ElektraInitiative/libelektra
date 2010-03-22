include (LibAddMacros)

file (GLOB HDR_FILES *.h)
add_headers (HDR_FILES)
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

file (GLOB SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

if (BUILD_SHARED)
	add_executable (kdb ${SOURCES})

	target_link_libraries (kdb elektra)
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_executable (kdb-full ${SOURCES})

	target_link_libraries (kdb-full elektra-full)
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_executable (kdb-static ${SOURCES})

	target_link_libraries (kdb-static elektra-static)
endif (BUILD_STATIC)
