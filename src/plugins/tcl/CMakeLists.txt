include (LibAddMacros)

set (SOURCES tcl.hpp tcl.cpp action.hpp action.cpp printer.hpp printer.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_cppheaders (SOURCES)

set (PLUGIN_NAME elektra-tcl)
add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)

install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
