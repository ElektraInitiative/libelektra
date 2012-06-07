include (LibAddMacros)

set (SOURCES null.h null.c)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-null MODULE ${SOURCES})
install (TARGETS elektra-null DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

#add_plugintest (null)
