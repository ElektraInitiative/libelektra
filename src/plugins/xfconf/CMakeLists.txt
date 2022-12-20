include (LibAddMacros)
include (FindXfconf)

if (NOT XFCONF_FOUND)
	remove_plugin (xfconf "libxfconf not found")
	return ()
endif ()

add_plugin (
	xfconf
	SOURCES xfconf.h xfconf.c
	INCLUDE_DIRECTORIES "${XFCONF_INCLUDE_DIRS}" "${CMAKE_CURRENT_BINARY_DIR}"
	LINK_ELEKTRA elektra-ease
	LINK_LIBRARIES "${XFCONF_LIBRARIES}"
	TEST_README COMPONENT libelektra${SO_VERSION}-experimental)

if (ADDTESTING_PHASE)
	include (LibAddTest)
	add_plugintest (xfconf MEMLEAK)
endif (ADDTESTING_PHASE)
