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
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/README.md" "${CMAKE_CURRENT_BINARY_DIR}/README.md")
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run-elektrad.sh" "${CMAKE_CURRENT_BINARY_DIR}/run-elektrad")
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run.sh" "${CMAKE_CURRENT_BINARY_DIR}/run-${tool}")
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/build.sh" "${CMAKE_CURRENT_BINARY_DIR}/build-${tool}")

		# install files to destination
		install (DIRECTORY elektrad webd client
			 DESTINATION ${install_directory}
			 PATTERN "node_modules" EXCLUDE
			 PATTERN "build" EXCLUDE)
		install (FILES README.md kdb.js DESTINATION ${install_directory})
		install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/run-elektrad
				  ${CMAKE_CURRENT_BINARY_DIR}/build-${tool}
			 DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

		# build elektra-web
		install (CODE "message (\"-- Installing: elektra-web > elektrad\")")
		install (
			CODE
				"execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/elektrad OUTPUT_QUIET)"
			)
		install (
			CODE
				"execute_process (COMMAND npm run build --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/elektrad OUTPUT_QUIET)"
			)
		install (
			CODE
				"execute_process (COMMAND npm prune --production --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/elektrad OUTPUT_QUIET)"
			)
		install (CODE "message (\"-- Installing: elektra-web > webd\")")
		install (
			CODE
				"execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/webd OUTPUT_QUIET)"
			)
		install (
			CODE
				"execute_process (COMMAND npm run build --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/webd OUTPUT_QUIET)"
			)
		install (
			CODE
				"execute_process (COMMAND npm prune --production --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/webd OUTPUT_QUIET)"
			)
		install (CODE "message (\"-- Installing: elektra-web > client\")")
		install (
			CODE
				"execute_process (COMMAND npm install --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/client OUTPUT_QUIET)"
			)
		install (
			CODE
				"execute_process (COMMAND npm run build --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/client OUTPUT_QUIET)"
			)
		install (
			CODE
				"execute_process (COMMAND npm prune --production --unsafe-perm WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${install_directory}/client OUTPUT_QUIET)"
			)

		# generate manpages
		generate_manpage (kdb-run-elektrad FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)
		generate_manpage (kdb-run-${tool} FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)

		# test apis
		find_program (DREDD_EXECUTABLE "dredd")
		if (DREDD_EXECUTABLE)

			# testing is possible
			add_test (NAME elektra-web COMMAND ./test.sh WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
			set_property (TEST elektra-web PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
		endif ()

	endif ()

endif ()
