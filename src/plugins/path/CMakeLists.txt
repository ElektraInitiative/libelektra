include (LibAddMacros)

set (SOURCES path.h path.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-path MODULE ${SOURCES})
install (TARGETS elektra-path DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

#add_plugintest (path)
