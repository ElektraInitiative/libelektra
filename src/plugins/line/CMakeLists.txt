include (LibAddMacros)

try_compile (COMPAT_GETLINE
	"${CMAKE_BINARY_DIR}"
	"${PROJECT_SOURCE_DIR}/src/plugins/line/testgetline.c"
	)

if (COMPAT_GETLINE)
	generate_readme (line)
	add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})
		
	set (SOURCES line.h line.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	set (PLUGIN_NAME elektra-line)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	target_link_libraries (${PLUGIN_NAME} elektra)

	install(DIRECTORY line DESTINATION ${TARGET_TEST_DATA_FOLDER})

	install (TARGETS ${PLUGIN_NAME}
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
else (COMPAT_GETLINE)
	remove_plugin (line "Unable to use getline function. Check libc version, should be higher than 4.6.27.")
endif (COMPAT_GETLINE)

add_plugintest (line)
