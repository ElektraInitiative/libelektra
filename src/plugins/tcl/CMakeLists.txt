include (LibAddMacros)

set (SOURCES tcl.hpp tcl.cpp action.hpp action.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_cppheaders (SOURCES)

add_library (elektra-tcl MODULE ${SOURCES})
install (TARGETS elektra-tcl DESTINATION lib/elektra)
