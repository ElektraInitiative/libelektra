include (LibAddMacros)

set (SOURCES glob.h glob.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-glob MODULE ${SOURCES})
install (TARGETS elektra-glob DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (glob)
