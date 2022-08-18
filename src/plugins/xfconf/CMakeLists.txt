include (LibAddMacros)

find_package (PkgConfig REQUIRED)

if (DEPENDENCY_PHASE)
        pkg_check_modules (XFCE QUIET libxfconf-0)

        if (NOT XFCE_FOUND)
                remove_plugin (xfconf "libxfconf-0 not found")
        else ()
                include_directories (${XFCE_INCLUDE_DIRS})
        endif ()
endif ()

add_plugin (
        xfconf
        SOURCES xfconf.h xfconf.c
        INCLUDE_DIRECTORIES "${XFCE_INCLUDE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}"
        LINK_ELEKTRA elektra-ease
        LINK_LIBRARIES ${XFCE_LIBRARIES}
        ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
