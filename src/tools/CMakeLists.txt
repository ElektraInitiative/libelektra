foreach (tool ${TOOLS})
	message (STATUS "Include Tool ${tool}")
	add_subdirectory ("${tool}")
endforeach (tool)
