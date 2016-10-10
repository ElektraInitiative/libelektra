set (install_directory ${TARGET_TOOL_EXEC_FOLDER}/${tool})

install (DIRECTORY public resources DESTINATION ${install_directory})
install (FILES bower.json Gruntfile.js package.json .bowerrc DESTINATION ${install_directory})
message (STATUS "Install directory: " ${install_directory})
install (CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory})")
