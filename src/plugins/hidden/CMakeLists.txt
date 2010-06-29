include(LibAddMacros)

set (SOURCES hidden.h hidden.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-hidden MODULE ${SOURCES})
install (TARGETS elektra-hidden DESTINATION lib/elektra)
