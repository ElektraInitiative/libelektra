include(LibAddMacros)

set (SOURCES doc.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

set (PLUGIN_NAME elektra-doc)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
