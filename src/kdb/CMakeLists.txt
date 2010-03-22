include (LibAddMacros)

file (GLOB HDR_FILES *.h)
add_headers (HDR_FILES)
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

file (GLOB SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

if (SHARED_BUILD)
	add_executable (kdb ${SOURCES})

	target_link_libraries (kdb elektra)
endif (SHARED_BUILD)

if (FULL_BUILD)
	add_executable (kdb-full ${SOURCES})

	target_link_libraries (kdb-full elektra-full)
endif (FULL_BUILD)

if (STATIC_BUILD)
	add_executable (kdb-static ${SOURCES})

	target_link_libraries (kdb-static elektra-static)
endif (STATIC_BUILD)
