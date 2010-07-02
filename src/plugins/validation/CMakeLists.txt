include (LibAddMacros)

set (SOURCES lookupre.c validation.h validation.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-validation MODULE ${SOURCES})
install (TARGETS elektra-validation DESTINATION lib/elektra)

add_plugintest (validation)
