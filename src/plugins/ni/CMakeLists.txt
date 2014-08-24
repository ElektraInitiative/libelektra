include (LibAddMacros)

file (GLOB NI_FILES ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/src/*)
file (GLOB NI_INI_FILES ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/src/tests/*.ini)

copy_file (${NI_INI_FILES} "${CMAKE_CURRENT_BINARY_DIR}")

set (NI_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/include ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/src/include)

add_includes (elektra-full ${NI_INCLUDE})
include_directories (${NI_INCLUDE})

generate_readme (ni)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

set (SOURCES ni.h ni.c ${NI_FILES})
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

set (PLUGIN_NAME elektra-ni)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (ni)
