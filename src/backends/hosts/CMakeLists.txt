include(LibAddMacros)

set (SOURCES hosts.h hosts.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-hosts MODULE ${SOURCES})
install_targets (/lib elektra-hosts)
