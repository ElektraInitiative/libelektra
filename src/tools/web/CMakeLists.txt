find_program (NPM_EXECUTABLE "npm")

list (FIND REMOVED_PLUGINS "yajl" yajl_index)
if (NOT yajl_index EQUAL -1)
    remove_tool (${tool} "yajl plugin not available")
else ()

if (NOT NPM_EXECUTABLE)
    remove_tool (${tool} "dependency manager 'npm' not found")
else ()
    # define configuration path
    set (config_root "user/sw/elektra/${tool}/#0/")
    set (config_default_profile "current")

    # find installation path
    set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})

    # configure and copy files
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
        "${CMAKE_CURRENT_BINARY_DIR}/README.md"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/run-elektrad.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/run-elektrad"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/run.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/run-${tool}"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/build.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/build-${tool}"
    )

    # install files to destination
    install (DIRECTORY elektrad webd client DESTINATION ${install_directory})
    install (FILES README.md kdb.js DESTINATION ${install_directory})
    install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/run-elektrad ${CMAKE_CURRENT_BINARY_DIR}/build-${tool}
             DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

    # attempt to install npm dependencies
    install (CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/elektrad OUTPUT_QUIET)")
    install (CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/client OUTPUT_QUIET)")

    # build elektra-web
    install (CODE "execute_process (COMMAND ${TARGET_TOOL_EXEC_FOLDER}/build-${tool} OUTPUT_QUIET)")

    generate_manpage (kdb-run-elektrad FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)
    generate_manpage (kdb-run-${tool} FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)

endif ()

endif ()

