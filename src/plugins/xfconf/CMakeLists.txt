include (LibAddMacros)
include (FindXfconf)

if (NOT XFCONF_FOUND)
	remove_plugin (xfconf "libxfconf not found")
	return ()
endif ()

add_plugin (
	xfconf
	SOURCES xfconf.h xfconf.c
	INCLUDE_DIRECTORIES "${GLIBCONFIG_INCLUDE_DIR}" "${GLIB_INCLUDE_DIR}" "${XFCONF_INCLUDE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}"
	LINK_ELEKTRA elektra-ease
	LINK_LIBRARIES "${GLIBCONFIG_LIBRARIES}" "${GLIB_LIBRARIES}" "${XFCONF_LIBRARIES}"
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
