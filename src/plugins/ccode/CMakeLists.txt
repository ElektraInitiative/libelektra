include (LibAddMacros)

set (SOURCES ccode.h ccode.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-ccode MODULE ${SOURCES})
install (TARGETS elektra-ccode DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (ccode)
