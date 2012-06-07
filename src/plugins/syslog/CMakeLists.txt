include(LibAddMacros)

set (SOURCES syslog.h syslog.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-syslog MODULE ${SOURCES})
install (TARGETS elektra-syslog DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
