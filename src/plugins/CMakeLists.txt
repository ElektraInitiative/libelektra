include(LibAddPlugin)

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
else ()
	generate_manpage (elektra-plugins FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md SECTION 7)
endif (NOT PLUGINS)

#TODO:
# - symlink for default plugin instead of having it in code?
#
#	shouldn't that be at installation time??
#	mkdir (${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
#	create_symlink (
#			libelektra-${BACKEND_DEFAULT}.so
#			${CMAKE_BINARY_DIR}/dist/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}/libelektra-default.so
#			)
#	message (STATUS "Default backend: ${BACKEND_DEFAULT}")
