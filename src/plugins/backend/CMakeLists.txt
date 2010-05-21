include(LibAddMacros)

set (SOURCES backend.h backend.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-backend MODULE ${SOURCES})
install (TARGETS elektra-backend DESTINATION lib/elektra)
