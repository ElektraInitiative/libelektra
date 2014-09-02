include(LibAddMacros)

generate_readme (dump)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

set (SOURCES dump.hpp dump.cpp)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

set (PLUGIN_NAME elektra-dump)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest(dump)
