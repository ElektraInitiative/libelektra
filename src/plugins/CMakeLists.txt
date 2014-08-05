foreach (plugin ${PLUGINS})
	if(${plugin} MATCHES ".*_.*")
		message(STATUS "Include Plugin's build variant ${plugin}")
	else()
		message(STATUS "Include Plugin ${plugin}")
		add_subdirectory ("${plugin}")
	endif()
endforeach(plugin)

# Check for plugins will be done here, because plugins might
# remove themselves.
#
# Note this is only a sanity check, not if elektra will really
# work with the chosen set of plugins.
if (NOT PLUGINS)
	message (SEND_ERROR "No plugins defined, please set PLUGINS with ccmake")
endif (NOT PLUGINS)

#TODO:
# - symlink for default plugin?
# - sanity check if dump/resolver is available
#
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
