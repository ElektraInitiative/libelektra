find_program (NPM_EXECUTABLE "npm")

if (NOT NPM_EXECUTABLE)
    remove_tool (${tool} "dependency manager 'npm' not found")
else ()

    # find installation path
    set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})

    # set variables for Gruntfile.js configuration
    set (REST_FRONTEND_SOURCE_INSTALL_REPOSITORY ${CMAKE_SOURCE_DIR})

    # configure and copy files
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/Gruntfile.js"
        "${CMAKE_CURRENT_BINARY_DIR}/Gruntfile.js"
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
        "${CMAKE_CURRENT_SOURCE_DIR}/configure.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/configure-${tool}"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
        "${CMAKE_CURRENT_BINARY_DIR}/README.md"
    )

    # install files to destination
    install (DIRECTORY public resources DESTINATION ${install_directory})
    install (FILES package.json README.md ${CMAKE_CURRENT_BINARY_DIR}/Gruntfile.js DESTINATION ${install_directory})
    if (NOT EXISTS ${install_directory}/application-config.json)
        install (FILES application-config.json DESTINATION ${install_directory})
    endif ()
    install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}
             ${CMAKE_CURRENT_BINARY_DIR}/configure-${tool} DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

    # attempt to install npm dependencies
    install (CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory})")

    generate_manpage (kdb-run-${tool} FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)

endif ()
