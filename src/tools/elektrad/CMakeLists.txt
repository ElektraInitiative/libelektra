find_program (GO_EXECUTABLE "go")

list (FIND REMOVED_PLUGINS "yajl" yajl_index)

if (NOT yajl_index EQUAL -1)
	remove_tool (elektrad "yajl plugin not available")
elseif (NOT GO_EXECUTABLE)
	remove_tool (elektrad "`go` executable was not found")
else ()
	# find installation path
	set (install_directory ${TARGET_TOOL_DATA_FOLDER}/elektrad)

	# configure and copy files
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/README.md" "${CMAKE_CURRENT_BINARY_DIR}/README.md")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run.sh" "${CMAKE_CURRENT_BINARY_DIR}/run-elektrad")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/build.sh" "${CMAKE_CURRENT_BINARY_DIR}/build-elektrad")

	# create temporary elektra.pc file
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/temp-elektra.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra.pc" @ONLY)

	# install files to destination
	install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-elektrad ${CMAKE_CURRENT_BINARY_DIR}/run-elektrad
			  ${CMAKE_CURRENT_BINARY_DIR}/build-elektrad DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

	if (BUILD_SHARED)
		set (ELEKTRA_LIBS_ELEKTRAD elektra-core elektra-kdb elektra-plugin elektra-ease elektra-meta)
	elseif (BUILD_FULL)
		set (ELEKTRA_LIBS_ELEKTRAD elektra-full)
	elseif (BUILD_STATIC)
		set (ELEKTRA_LIBS_ELEKTRAD elektra-static)
	endif (BUILD_SHARED)

	add_custom_command (
		OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad"
		COMMAND ${CMAKE_COMMAND} ARGS -E env GO111MODULE=on PKG_CONFIG_PATH=${CMAKE_CURRENT_BINARY_DIR} ${GO_EXECUTABLE} build -o
			"${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad"
		COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad"
			"${CMAKE_BINARY_DIR}/bin/elektrad"
		VERBATIM
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
		DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/main.go" ${ELEKTRA_LIBS_ELEKTRAD})
	add_custom_target (elektrad ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad")

	# build elektra-web
	install (CODE "message (\"-- Installing: elektra-web > elektrad\")")
	install (PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/elektrad/elektrad" DESTINATION "${install_directory}")

	# generate man pages
	generate_man_page (kdb-run-elektrad FILENAME "${CMAKE_CURRENT_BINARY_DIR}/README.md" GENERATED_FROM
			   "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

	# test apis
	find_program (DREDD_EXECUTABLE "dredd")
	if (DREDD_EXECUTABLE)
		# testing is possible
		add_test (
			NAME test_elektra_web
			COMMAND test.sh
			WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
		set_property (TEST test_elektra_web PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib"
							     "PKG_CONFIG_PATH=${CMAKE_CURRENT_BINARY_DIR}")
	else (DREDD_EXECUTABLE)
		message (WARNING "Exclude tests for `elektrad`, since `dredd` was not found.")
	endif (DREDD_EXECUTABLE)

endif ()
