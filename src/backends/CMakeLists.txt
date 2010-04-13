foreach (backend ${BACKENDS})
	message("-- Include Backend ${backend}")
	add_subdirectory ("${backend}")
endforeach(backend)

list (FIND BACKENDS ${BACKEND_DEFAULT} output)
if (output GREATER -1)
	mkdir (${CMAKE_BINARY_DIR}/dist/lib/elektra)
	create_symlink (
			libelektra-${BACKEND_DEFAULT}.so
			${CMAKE_BINARY_DIR}/dist/lib/elektra/libelektra-default.so
			)
else (output GREATER -1)
	message (SEND_ERROR "default backend ${BACKEND_DEFAULT} is not included")
endif (output GREATER -1)

install (DIRECTORY ${CMAKE_BINARY_DIR}/dist/lib/elektra DESTINATION lib)
