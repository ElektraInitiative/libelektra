include(LibAddMacros)

set (SOURCES template.h template.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-template MODULE ${SOURCES})
install (TARGETS elektra-template DESTINATION lib/elektra)
