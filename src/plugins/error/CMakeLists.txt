include(LibAddMacros)

set (SOURCES error.h error.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-error MODULE ${SOURCES})
install (TARGETS elektra-error DESTINATION lib/elektra)
