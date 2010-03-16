include(LibAddMacros)

set (SOURCES filesys.h filesys.c)
add_headers(SOURCES)

add_library(elektra-filesys MODULE ${SOURCES})
install_targets (/lib elektra-filesys)
