find_program (NPM_EXECUTABLE "npm")

if (NPM_EXECUTABLE)
    set (install_directory ${TARGET_TOOL_EXEC_FOLDER}/${tool})

    install (DIRECTORY public resources DESTINATION ${install_directory})
    install (FILES Gruntfile.js package.json application-config.json DESTINATION ${install_directory})
    install (CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory})")
endif (NPM_EXECUTABLE)