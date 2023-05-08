find_program (GO_EXECUTABLE go)

if (GO_EXECUTABLE)
	add_binding (go-elektra)

	# build
	add_custom_command (
		OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/lib/libelektra.golib"
		COMMAND ${GO_EXECUTABLE} build ./kdb
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

	# tests
	add_custom_command (
		OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/tests/target/release"
		COMMAND ${GO_EXECUTABLE} test ./kdb
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

else ()
	exclude_binding (go-elektra, "go not found")
	return ()
endif ()
