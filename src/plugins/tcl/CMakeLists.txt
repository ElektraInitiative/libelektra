find_package(Boost)
if (Boost_FOUND)
	include (LibAddMacros)

	generate_readme (tcl)
	add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	set (SOURCES tcl.hpp tcl.cpp action.hpp action.cpp printer.hpp printer.cpp)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)
	add_cppheaders (SOURCES)
	include_directories (${Boost_INCLUDE_DIRS})

	set (PLUGIN_NAME elektra-tcl)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	target_link_libraries (${PLUGIN_NAME} elektra)

	install (TARGETS ${PLUGIN_NAME}
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
else (Boost_FOUND)
        remove_plugin (tcl "boost not found")
endif (Boost_FOUND)
