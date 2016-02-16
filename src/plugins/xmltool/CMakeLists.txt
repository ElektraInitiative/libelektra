find_package (LibXml2)

if (LIBXML2_FOUND)
	include (LibAddMacros)

	add_plugin(xmltool
		SOURCES
			xmltool.h
			xmltool.c
			stream.c
			kdbtools.c
			kscompare.c
		INCLUDE_DIRECTORIES
			${LIBXML2_INCLUDE_DIR}
		LINK_LIBRARIES
			elektra-meta
			${LIBXML2_LIBRARIES}
		)

	add_plugintest (xmltool
		MEMLEAK)

	install(DIRECTORY xmltool DESTINATION ${TARGET_TEST_DATA_FOLDER})

else (LIBXML2_FOUND)
	remove_plugin (xmltool "libxml2 not found")
endif (LIBXML2_FOUND)

