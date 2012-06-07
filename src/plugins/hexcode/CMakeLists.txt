include (LibAddMacros)

set (SOURCES hexcode.h hexcode.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-hexcode MODULE ${SOURCES})
install (TARGETS elektra-hexcode DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (hexcode)
