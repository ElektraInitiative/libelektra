include(LibAddMacros)

set (SOURCES dump.hpp dump.cpp)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-dump MODULE ${SOURCES})
target_link_libraries (elektra-dump elektra)
install (TARGETS elektra-dump DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(dump)
