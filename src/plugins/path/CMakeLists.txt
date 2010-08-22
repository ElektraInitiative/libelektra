include (LibAddMacros)

set (SOURCES path.h path.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-path MODULE ${SOURCES})
install (TARGETS elektra-path DESTINATION lib/elektra)

#add_plugintest (path)
