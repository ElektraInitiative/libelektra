include (LibAddMacros)

set (SOURCES type.hpp type.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_cppheaders (SOURCES)

add_library (elektra-type MODULE ${SOURCES})
install (TARGETS elektra-type DESTINATION lib/elektra)

#add_plugintest (type)
