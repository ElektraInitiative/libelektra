include (LibAddMacros)

set (SOURCES hexcode.h hexcode.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-hexcode MODULE ${SOURCES})
install (TARGETS elektra-hexcode DESTINATION lib/elektra)

add_plugintest (hexcode)
