foreach (plugin ${PLUGINS})
	message("-- Include Plugin ${plugin}")
	add_subdirectory ("${plugin}")
endforeach(plugin)

#symlink for default plugin?
#list (FIND BACKENDS ${BACKEND_DEFAULT} output)
#if (output GREATER -1)
#	mkdir (${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
#	create_symlink (
#			libelektra-${BACKEND_DEFAULT}.so
#			${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}/libelektra-default.so
#			)
#	message ("-- Default backend: ${BACKEND_DEFAULT}")
#else (output GREATER -1)
#	message (SEND_ERROR "default backend ${BACKEND_DEFAULT} is not included")
#endif (output GREATER -1)

#install (DIRECTORY ${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER} DESTINATION lib)
