include(LibAddMacros)

set (SOURCES hosts.h hosts.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-hosts MODULE ${SOURCES})
install (TARGETS elektra-hosts DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(hosts)
