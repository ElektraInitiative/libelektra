include (LibAddMacros)

find_package (ZeroMQ QUIET)

if (NOT ZeroMQ_FOUND)
	remove_tool (${tool} "libzmq not found")
else () # find installation path
	set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})

	set (SRC_FILES hub-zeromq.c)

	set (SOURCES ${SRC_FILES} ${ELEKTRA_HEADERS})

	# Build hub
	set (HUB hub-zeromq)

	add_executable (${HUB} ${SOURCES})

	target_link_libraries (${HUB} ${ZeroMQ_LIBRARIES})
	target_link_elektra (${HUB} elektra-kdb)
	target_include_directories (${HUB} SYSTEM PUBLIC ${ZeroMQ_INCLUDE_DIRS})

	# configure and copy files
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/run.sh" "${CMAKE_CURRENT_BINARY_DIR}/run-${tool}")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/stop.sh" "${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}")
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/README.md" "${CMAKE_CURRENT_BINARY_DIR}/README.md") # copy hub binary
	install (TARGETS ${HUB} DESTINATION ${install_directory})

	# install files to destination
	install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}
		 DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

	generate_man_page (kdb-run-${tool} FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
endif ()
