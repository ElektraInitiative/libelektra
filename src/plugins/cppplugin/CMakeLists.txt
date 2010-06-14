include(LibAddMacros)

set (SOURCES cppplugin.hpp cppplugin.cpp)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)
add_cppheaders(SOURCES)

add_library(elektra-cppplugin MODULE ${SOURCES})
install (TARGETS elektra-cppplugin DESTINATION lib/elektra)
