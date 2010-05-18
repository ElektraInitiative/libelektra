include(LibAddMacros)

set (SOURCES filesys.h filesys.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-filesys MODULE ${SOURCES})
install (TARGETS elektra-filesys DESTINATION lib/elektra)
