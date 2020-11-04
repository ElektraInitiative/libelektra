foreach (tool ${TOOLS})
	message (STATUS "Include tool ${tool}")

	if (${tool} STREQUAL "web")
		add_subdirectory ("elektrad")
		add_subdirectory ("webui")
		add_subdirectory ("webd")
	else ()
		add_subdirectory ("${tool}")
	endif (${tool} STREQUAL "web")

endforeach (tool)
