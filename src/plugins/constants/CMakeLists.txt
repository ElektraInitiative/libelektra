include (LibAddMacros)


configure_file(constants.c.in ${CMAKE_BINARY_DIR}/constants.c)

set (SOURCES ${CMAKE_BINARY_DIR}/constants.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

set (PLUGIN_NAME elektra-constants)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

#add_plugintest (constants)
