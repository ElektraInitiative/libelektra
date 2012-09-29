include (LibAddMacros)

set (SOURCES simpleini.h simpleini.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-simpleini MODULE ${SOURCES})
install (TARGETS elektra-simpleini DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_scripttest(simpleini.sh)

#add_plugintest (simpleini)
