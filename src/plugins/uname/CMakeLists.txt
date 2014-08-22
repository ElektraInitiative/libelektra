try_compile(HAS_UNAME
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/uname/testuname.c"
		)

include(LibAddMacros)

if(HAS_UNAME)

	generate_readme (uname)
	add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	set (SOURCES uname.h uname.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	set (PLUGIN_NAME elektra-uname)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	target_link_libraries (${PLUGIN_NAME} elektra)
	install (TARGETS ${PLUGIN_NAME} DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	add_plugintest (uname)
else()
	remove_plugin (uname "uname is missing")
endif()
