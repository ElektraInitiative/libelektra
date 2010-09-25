find_package (LibXml2 REQUIRED)
if (LIBXML2_FOUND)
	include (LibAddMacros)

	add_includes (elektra-full ${LIBXML2_INCLUDE_DIR})
	include_directories (${LIBXML2_INCLUDE_DIR})

	set (SOURCES xmltool.h xmltool.c stream.c kdbtools.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	add_library (elektra-xmltool MODULE ${SOURCES})
	install (TARGETS elektra-xmltool DESTINATION lib/elektra)

	target_link_libraries (elektra-xmltool ${LIBXML2_LIBRARIES})
	add_libraries(elektra-full ${LIBXML2_LIBRARIES})

	add_subdirectory (tests)
else (LIBXML2_FOUND)
	remove_plugin (xmltool "libxml2 not found")
endif (LIBXML2_FOUND)

