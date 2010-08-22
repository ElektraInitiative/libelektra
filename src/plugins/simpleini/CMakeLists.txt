include (LibAddMacros)

set (SOURCES simpleini.h simpleini.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-simpleini MODULE ${SOURCES})
install (TARGETS elektra-simpleini DESTINATION lib/elektra)

#add_plugintest (simpleini)
