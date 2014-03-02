foreach (tool ${TOOLS})
	message("-- Include Tool ${tool}")
	add_subdirectory ("${tool}")
endforeach(tool)
