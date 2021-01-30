if (DEPENDENCY_PHASE)
	find_package (LibXml2 QUIET)

	if (NOT LIBXML2_FOUND)
		remove_plugin (xmltool "libxml2 not found")
	endif ()
endif ()

add_plugin (
	xmltool
	SOURCES xmltool.h xmltool.c stream.c kdbtools.c kscompare.c
	INSTALL_TEST_DATA
	INCLUDE_DIRECTORIES ${LIBXML2_INCLUDE_DIR}
	LINK_ELEKTRA elektra-meta
	LINK_LIBRARIES ${LIBXML2_LIBRARIES} COMPONENT libelektra${SO_VERSION}-xmltool)

if (ADDTESTING_PHASE)
	include (LibAddMacros)

	add_plugintest (xmltool INSTALL_TEST_DATA MEMLEAK)
endif ()
