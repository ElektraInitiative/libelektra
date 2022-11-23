find_program (NPM_EXECUTABLE "npm")

list (FIND REMOVED_PLUGINS "yajl" yajl_index)

if (NOT yajl_index EQUAL -1)
	remove_tool (webd "yajl plugin not available")
elseif (NOT NPM_EXECUTABLE)
	remove_tool (webd "`npm` executable was not found")
else ()

	# define configuration path
	set (config_root "user:/sw/elektra/webd/#0/")
	set (config_default_profile "current")

	# find installation path
	set (install_directory ${TARGET_TOOL_DATA_FOLDER}/webd)

	# configure and copy files
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/README.md" "${CMAKE_CURRENT_BINARY_DIR}/README.md")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run.sh" "${CMAKE_CURRENT_BINARY_DIR}/run-webd")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/build.sh" "${CMAKE_CURRENT_BINARY_DIR}/build-webd")

	# install files to destination
	install (
		DIRECTORY .
		DESTINATION ${install_directory}
		PATTERN "node_modules" EXCLUDE
		PATTERN "build" EXCLUDE)

	install (FILES README.md DESTINATION ${install_directory})
	install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-webd ${CMAKE_CURRENT_BINARY_DIR}/build-webd
		 DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

	# build elektra-web
	install (CODE "message (\"-- Installing: elektra-web > webd\")")
	install (
		CODE "execute_process (COMMAND npm install --no-audit --no-save --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)
	install (
		CODE "execute_process (COMMAND npm run build --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)
	install (
		CODE "execute_process (COMMAND npm prune --production --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory} OUTPUT_QUIET)"
	)

	# generate manpages
	generate_manpage (kdb-run-webd FILENAME "${CMAKE_CURRENT_BINARY_DIR}/README.md" GENERATED_FROM
			  "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

endif ()
