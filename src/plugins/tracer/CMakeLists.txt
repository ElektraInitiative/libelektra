include(LibAddMacros)

set (SOURCES tracer.h tracer.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-tracer MODULE ${SOURCES})
install (TARGETS elektra-tracer DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
