find_package (LibXml2 REQUIRED)
if (LIBXML2_FOUND)
	include (LibAddMacros)

	add_includes (elektra-full ${LIBXML2_INCLUDE_DIR})
	include_directories (${LIBXML2_INCLUDE_DIR})

	set (SOURCES xmltool.h xmltool.c stream.c kdbtools.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	set (PLUGIN_NAME elektra-xmltool)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	target_link_libraries (${PLUGIN_NAME} elektra)
	install (TARGETS ${PLUGIN_NAME} DESTINATION
		lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
	target_link_libraries (${PLUGIN_NAME} ${LIBXML2_LIBRARIES})

	add_libraries(elektra-full ${LIBXML2_LIBRARIES})

	add_plugintest (xmltool)

	#xmllib2 leaks memory
	set_property(TEST testmod_xmltool PROPERTY LABELS memleak)

	install(DIRECTORY xmltool DESTINATION ${TARGET_TEST_DATA_FOLDER})
else (LIBXML2_FOUND)
	remove_plugin (xmltool "libxml2 not found")
endif (LIBXML2_FOUND)

