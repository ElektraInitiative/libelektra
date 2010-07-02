include (LibAddMacros)

set (SOURCES xmltool.h xmltool.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-xmltool MODULE ${SOURCES})
install (TARGETS elektra-xmltool DESTINATION lib/elektra)

add_plugintest (xmltool)
