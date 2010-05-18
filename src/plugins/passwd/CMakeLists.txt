include(LibAddMacros)

set (SOURCES passwd.h passwd.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-passwd MODULE ${SOURCES})
install (TARGETS elektra-passwd DESTINATION lib/elektra)
