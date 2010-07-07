include(LibAddMacros)

set (SOURCES dump.hpp dump.cpp)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-dump MODULE ${SOURCES})
install (TARGETS elektra-dump DESTINATION lib/elektra)

add_plugintest(dump)
