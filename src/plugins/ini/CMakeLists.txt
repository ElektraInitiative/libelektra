include (LibAddMacros)


set (SOURCES ini.h ini.c lib/inih.h lib/inih.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_definitions( -DINI_ALLOW_MULTILINE=0 )
set (PLUGIN_NAME elektra-ini)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

generate_readme (ini)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

install(DIRECTORY ini DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest (ini)
