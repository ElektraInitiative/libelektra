foreach (backend ${BACKENDS})
	message("-- Include Backend ${backend}")
	add_subdirectory ("${backend}")
endforeach(backend)
