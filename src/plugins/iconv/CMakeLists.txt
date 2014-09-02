include (LibAddMacros)

generate_readme (iconv)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

set (SOURCES iconv.h iconv.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

set (PLUGIN_NAME elektra-iconv)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (iconv)
