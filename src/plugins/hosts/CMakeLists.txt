include(LibAddMacros)

generate_readme (hosts)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

set (SOURCES hosts.h hosts.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

set (PLUGIN_NAME elektra-hosts)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install(DIRECTORY hosts DESTINATION ${TARGET_TEST_DATA_FOLDER})

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(hosts)
