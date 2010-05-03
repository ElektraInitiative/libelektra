include (LibAddMacros)

file (GLOB HDR_FILES *.h)
add_headers (HDR_FILES)
include_directories (${CMAKE_CURRENT_SOURCE_DIR})

file (GLOB SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

if (BUILD_SHARED)
	add_executable (kdb ${SOURCES})

	target_link_libraries (kdb elektra)

	install (TARGETS kdb DESTINATION bin)
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_executable (kdb-full ${SOURCES})

	if (BUILD_TOOLS)
		target_link_libraries (kdb-full elektra-full elektratools)
	else (BUILD_TOOLS)
		target_link_libraries (kdb-full elektra-full)
	endif (BUILD_TOOLS)

	install (TARGETS kdb-full DESTINATION bin)
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_executable (kdb-static ${SOURCES})

	set_target_properties (kdb-static PROPERTIES LINKER_LANGUAGE CXX)

	if (BUILD_TOOLS)
		target_link_libraries (kdb-static elektra-static elektratools-static elektra-static)
	else (BUILD_TOOLS)
		target_link_libraries (kdb-static elektra-static)
	endif (BUILD_TOOLS)

	install (TARGETS kdb-static DESTINATION bin)
endif (BUILD_STATIC)
