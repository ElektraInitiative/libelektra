include (LibAddMacros)

set (SOURCES iconv.h iconv.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-iconv MODULE ${SOURCES})
install (TARGETS elektra-iconv DESTINATION lib/elektra)

add_plugintest (iconv)
