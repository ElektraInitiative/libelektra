include(LibAddMacros)

set (SOURCES doc.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-doc MODULE ${SOURCES})
install (TARGETS elektra-doc DESTINATION lib/elektra)
