include (LibAddMacros)

generate_readme (struct)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

set (SOURCES struct.hpp struct.cpp checker.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_cppheaders (SOURCES)

set (PLUGIN_NAME elektra-struct)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

file (GLOB TESTS testmod_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_cpp_plugintest(${name})
endforeach (file ${TESTS})
