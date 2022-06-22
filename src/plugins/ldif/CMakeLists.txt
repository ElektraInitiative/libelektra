include (LibAddPlugin)

if (DEPENDENCY_PHASE)

        # Use Homebrew version of OpenLDAP on macOS, if OpenLDAP is installed and `OPENLDAP_ROOT_DIR` is not set.
        if (APPLE AND NOT DEFINED OPENLDAP_ROOT_DIR)
                execute_process (
                        COMMAND brew list openldap
                        RESULT_VARIABLE FAILURE
                        OUTPUT_QUIET ERROR_QUIET)
                if (NOT FAILURE)
                        set (OPENLDAP_ROOT_DIR /usr/local/opt/openldap)
                endif (NOT FAILURE)
        endif (APPLE AND NOT DEFINED OPENLDAP_ROOT_DIR)

        find_package (OpenLdap QUIET)
        if (NOT OPENLDAP_FOUND)
                remove_plugin (ldif "Cannot find openldap library")
        endif ()
endif ()

add_plugin (
        ldif
        SOURCES ldif.h ldif.c
        INCLUDE_DIRECTORIES ${OPENLDAP_INCLUDE_DIRS}
        LINK_ELEKTRA elektra-ease
        LINK_LIBRARIES ${OPENLDAP_LIBRARIES}
        ADD_TEST INSTALL_TEST_DATA TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
