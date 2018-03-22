find_program (NPM_EXECUTABLE "npm")

if (NOT NPM_EXECUTABLE)
    remove_tool (${tool} "dependency manager 'npm' not found")
else ()

    # create simplified tool name without special characters
    string (REGEX REPLACE "[^a-zA-Z]" "" tool_name_simplified ${tool})

    # define configuration path
    set (config_root "user/sw/elektra/${tool_name_simplified}/#0/")
    set (config_default_profile "current")

    # find installation path
    set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})

    # configure and copy files
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
        "${CMAKE_CURRENT_BINARY_DIR}/README.md"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/run.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/run-${tool}"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/stop.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/build.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/build-${tool}"
    )

    # install files to destination
    install (DIRECTORY elektrad webd client DESTINATION ${install_directory})
    install (FILES README.md kdb.js DESTINATION ${install_directory})
    install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}
             ${CMAKE_CURRENT_BINARY_DIR}/build-${tool}
             DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

    generate_manpage (kdb-run-${tool} FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)

endif ()
