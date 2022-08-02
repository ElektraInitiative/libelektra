include (LibAddMacros)

find_package (PkgConfig REQUIRED)

pkg_check_modules (XFCE REQUIRED libxfconf-0)

include_directories (${XFCE_INCLUDE_DIRS})

add_plugin (
        xfconf
        SOURCES xfconf.h xfconf.c
        INCLUDE_DIRECTORIES "${XFCE_INCLUDE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}"
        LINK_ELEKTRA elektra-ease
        LINK_LIBRARIES ${XFCE_LIBRARIES}
        ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
