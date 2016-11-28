# find installation path
set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})

# set variables for Gruntfile.js configuration
set (REST_FRONTEND_SOURCE_INSTALL_REPOSITORY ${CMAKE_SOURCE_DIR})

# configure and copy Gruntfile.js
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/Gruntfile.js ${CMAKE_CURRENT_BINARY_DIR}/Gruntfile.js)

# install files to destination
install (DIRECTORY public resources DESTINATION ${install_directory})
install (FILES package.json README.md ${CMAKE_CURRENT_BINARY_DIR}/Gruntfile.js DESTINATION ${install_directory})
if (NOT EXISTS ${install_directory}/application-config.json)
    install (FILES application-config.json DESTINATION ${install_directory})
endif ()

# attempt to install npm dependencies
find_program (NPM_EXECUTABLE "npm")
if (NPM_EXECUTABLE)
    install (CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory})")
endif (NPM_EXECUTABLE)
