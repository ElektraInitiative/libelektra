include (LibAddMacros)


set (SOURCES noresolver.h noresolver.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

generate_readme (noresolver)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

set (PLUGIN_NAME elektra-noresolver)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

#add_plugintest (noresolver)
