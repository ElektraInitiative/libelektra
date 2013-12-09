include(LibAddMacros)

set (SOURCES resolver.h resolver.c filename.c lock.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

#this needs posix, so it won't compile with c99 standard
#the last -std will be used, so it should be no problem
#that this is only added here
# this would not solve the problem for -full and -static..
#set_target_properties (elektra-resolver PROPERTIES COMPILE_FLAGS "-std=gnu99")

set (PLUGIN_NAME elektra-resolver)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(resolver)
