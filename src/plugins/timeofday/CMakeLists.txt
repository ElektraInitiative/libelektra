include(LibAddMacros)

set (SOURCES timeofday.h timeofday.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

set (PLUGIN_NAME elektra-timeofday)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME} DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
