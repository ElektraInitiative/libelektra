find_program (GO_EXECUTABLE go)

if (GO_EXECUTABLE)
	add_binding (go-elektra)

	# create temporary elektra.pc file
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/temp-elektra.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra.pc" @ONLY)

	# build
	add_custom_command (
		OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/lib/libelektra.golib"
		COMMAND ${CMAKE_COMMAND} ARGS -E env GO111MODULE=on PKG_CONFIG_PATH=${CMAKE_CURRENT_BINARY_DIR} ${GO_EXECUTABLE} build
			./kdb
		WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

	# tests
	add_custom_command (
		OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/tests/target/release"
		COMMAND ${GO_EXECUTABLE} test ./kdb
		WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

else ()
	exclude_binding (go-elektra, "go not found")
	return ()
endif ()
