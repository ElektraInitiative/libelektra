include (LibAddMacros)

file (GLOB NI_FILES ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/src/*)
file (GLOB NI_INI_FILES ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/src/tests/*.ini)

copy_file (${NI_INI_FILES} "${CMAKE_CURRENT_BINARY_DIR}")

set (NI_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/include ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0/src/include)

add_includes (elektra-full ${NI_INCLUDE})
include_directories (${NI_INCLUDE})

set (SOURCES ni.h ni.c ${NI_FILES})
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)

add_library (elektra-ni MODULE ${SOURCES})
install (TARGETS elektra-ni DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_plugintest (ni)
