include(LibAddMacros)

set (SOURCES hosts.h hosts.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

configure_file (hosts ${CMAKE_CURRENT_BINARY_DIR}/hosts)

set (PLUGIN_NAME elektra-hosts)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(hosts)
