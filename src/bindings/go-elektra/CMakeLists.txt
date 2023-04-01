find_program (GO_EXECUTABLE go)

if (GO_EXECUTABLE)
	add_binding (go-elektra)

	add_custom_command (COMMAND ${GO_EXECUTABLE} build ./kdb WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
else ()
	exclude_binding (go-elektra, "elektra not found")
	return ()
endif ()
