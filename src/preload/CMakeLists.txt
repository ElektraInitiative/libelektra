set (SOURCES preload.c)

include (LibAddMacros)
add_headers (SOURCES)

if (BUILD_SHARED)
	add_executable (preload ${SOURCES})
	target_link_libraries (preload elektra)
	install (TARGETS preload DESTINATION bin)
endif (BUILD_SHARED)

if (BUILD_FULL)
	add_executable (preload-full ${SOURCES})
	target_link_libraries (preload-full elektra-full)
	install (TARGETS preload-full DESTINATION bin)
endif (BUILD_FULL)

if (BUILD_STATIC)
	add_executable (preload-static ${SOURCES})
	target_link_libraries (preload-static elektra-static)
	install (TARGETS preload-static DESTINATION bin)
endif (BUILD_STATIC)
