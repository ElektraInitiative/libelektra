find_program (NPM_EXECUTABLE "npm")

if (NOT NPM_EXECUTABLE)
	remove_tool (${tool} "dependency manager 'npm' not found")
else ()

	# create simplified tool name without special characters
	string (REGEX REPLACE "[^a-zA-Z]" "" tool_name_simplified ${tool})

	# define configuration path
	set (config_root "/sw/elektra/${tool_name_simplified}/#0/")
	set (config_default_profile "current")

	# find installation path
	set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})

	# set variables for Gruntfile.js configuration
	set (REST_FRONTEND_SOURCE_INSTALL_REPOSITORY ${CMAKE_SOURCE_DIR})

	# configure and copy files
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/Gruntfile.js" "${CMAKE_CURRENT_BINARY_DIR}/Gruntfile.js")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run.sh" "${CMAKE_CURRENT_BINARY_DIR}/run-${tool}")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/stop.sh" "${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/build.sh" "${CMAKE_CURRENT_BINARY_DIR}/build-${tool}")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/mount-config.sh" "${CMAKE_CURRENT_BINARY_DIR}/mount-${tool}-config")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/README.md" "${CMAKE_CURRENT_BINARY_DIR}/README.md")

	# install files to destination TODO: make clear via repo structure that this happens
	install (DIRECTORY public resources DESTINATION ${install_directory})
	install (FILES package.json README.md ${CMAKE_CURRENT_BINARY_DIR}/Gruntfile.js application-config.json
		 DESTINATION ${install_directory})
	install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}
			  ${CMAKE_CURRENT_BINARY_DIR}/build-${tool} ${CMAKE_CURRENT_BINARY_DIR}/mount-${tool}-config
		 DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

	# attempt to install npm dependencies
	install (
		CODE "execute_process (COMMAND npm install --no-audit --no-save --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)

	generate_man_page (kdb-run-${tool} FILENAME "${CMAKE_CURRENT_BINARY_DIR}/README.md" GENERATED_FROM
			   "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

endif ()
