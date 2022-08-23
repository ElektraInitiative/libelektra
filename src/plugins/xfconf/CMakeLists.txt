include (LibAddMacros)
include (FindXfconf)

if (NOT XFCONF_FOUND)
        remove_plugin (xfconf "libxfconf not found")
        return ()
endif ()

add_plugin (
        xfconf
        SOURCES xfconf.h xfconf.c
        INCLUDE_DIRECTORIES "${XFCONF_INCLUDE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}"
        LINK_ELEKTRA elektra-ease
        LINK_LIBRARIES ${XFCONF_LIBRARIES}
        ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
