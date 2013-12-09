include(LibAddMacros)

set (SOURCES resolver.h resolver.c filename.c lock.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-resolver MODULE ${SOURCES})
target_link_libraries (elektra-resolver elektra)
#this needs posix, so it won't compile with c99 standard
#the last -std will be used, so it should be no problem
#that this is only added here
#set_target_properties (elektra-resolver PROPERTIES COMPILE_FLAGS "-std=gnu99")
install (TARGETS elektra-resolver DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(resolver)
