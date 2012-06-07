include (LibAddMacros)

set (SOURCES network.h network.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-network MODULE ${SOURCES})
install (TARGETS elektra-network DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (network)
