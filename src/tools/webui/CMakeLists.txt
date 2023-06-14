find_program (NPM_EXECUTABLE "npm")

if (NOT NPM_EXECUTABLE)
	remove_tool (webui "`npm` executable was not found")
else ()

	# configure and copy files
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/README.md" "${CMAKE_CURRENT_BINARY_DIR}/README.md")

	# find installation path
	set (install_directory ${TARGET_TOOL_DATA_FOLDER}/webui)

	# install files to destination TODO: make clear via repo structure that this happens
	install (
		DIRECTORY .
		DESTINATION ${install_directory}
		PATTERN "node_modules" EXCLUDE
		PATTERN "build" EXCLUDE)

	# TODO: isn't this redundant?
	install (FILES README.md DESTINATION ${install_directory})

	install (CODE "message (\"-- Installing: elektra-web > webui\")")
	install (
		CODE "execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)
	install (
		CODE "execute_process (COMMAND npm run build --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)
	install (
		CODE "execute_process (COMMAND npm prune --production --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)

endif ()
