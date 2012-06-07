include(LibAddMacros)

set (SOURCES timeofday.h timeofday.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-timeofday MODULE ${SOURCES})

install (TARGETS elektra-timeofday DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
