find_program (GO_EXECUTABLE "go")
find_program (NPM_EXECUTABLE "npm")

list (FIND REMOVED_PLUGINS "yajl" yajl_index)
if (NOT yajl_index EQUAL -1)
	remove_tool (${tool} "yajl plugin not available")
else ()
	if (NOT NPM_EXECUTABLE)
		remove_tool (${tool} "`npm` executable was not found")
	elseif (NOT GO_EXECUTABLE)
		remove_tool (${tool} "`go` executable was not found")
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

		# create temporary elektra.pc file
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektrad/elektra.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektra.pc"
				@ONLY)

		# install files to destination
		install (DIRECTORY webd client DESTINATION ${install_directory} PATTERN "node_modules" EXCLUDE PATTERN "build" EXCLUDE)
		install (FILES README.md kdb.js DESTINATION ${install_directory})
		install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/run-elektrad
				  ${CMAKE_CURRENT_BINARY_DIR}/build-${tool}
			 DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

		if (BUILD_SHARED)
			set (ELEKTRA_LIBS_ELEKTRAD
			     elektra-core
			     elektra-kdb
			     elektra-plugin
			     elektra-ease
			     elektra-meta)
		elseif (BUILD_FULL)
			set (ELEKTRA_LIBS_ELEKTRAD elektra-full)
		elseif (BUILD_STATIC)
			set (ELEKTRA_LIBS_ELEKTRAD elektra-static)
		endif (BUILD_SHARED)
		add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad"
				    COMMAND ${CMAKE_COMMAND}
					    ARGS
					    -E
					    env
					    GO111MODULE=on
					    PKG_CONFIG_PATH=${CMAKE_CURRENT_BINARY_DIR}/elektrad
					    ${GO_EXECUTABLE}
					    build
					    -o
					    "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad"
				    VERBATIM
				    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/elektrad"
				    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/elektrad/main.go" ${ELEKTRA_LIBS_ELEKTRAD})
		add_custom_target (elektrad ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad")

		# build elektra-web
		install (CODE "message (\"-- Installing: elektra-web > elektrad\")")
		install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad" DESTINATION "${install_directory}")
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
			configure_file (test.sh test.sh @ONLY)
			add_test (NAME test_elektra_web
				  COMMAND "${CMAKE_CURRENT_BINARY_DIR}/test.sh"
				  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
			set_property (TEST test_elektra_web
				      PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib"
					       "PKG_CONFIG_PATH=${CMAKE_CURRENT_BINARY_DIR}/elektrad")
		else (DREDD_EXECUTABLE)
			message (WARNING "Exclude tests for `elektrad`, since `dredd` was not found.")
		endif (DREDD_EXECUTABLE)

	endif ()

endif ()
