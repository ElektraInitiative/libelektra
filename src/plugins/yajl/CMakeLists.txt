find_package(Yajl REQUIRED)

if (YAJL_FOUND)
	include (LibAddMacros)

	#set(ELEKTRA_YAJL_VERBOSE yes)

	generate_readme (yajl)

	add_includes (elektra-full ${YAJL_INCLUDE_DIR})
	include_directories (${YAJL_INCLUDE_DIR})

	add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	set (SOURCES yajl.c iterator.c yajl_gen.c
		yajl_gen_open.c  yajl_gen_close.c
		yajl_parse.c name.c)

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/yajl.h.in"
		"${CMAKE_CURRENT_BINARY_DIR}/yajl.h"
		)

	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	install(DIRECTORY yajl DESTINATION ${TARGET_TEST_DATA_FOLDER})

	set (PLUGIN_NAME elektra-yajl)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	install (TARGETS ${PLUGIN_NAME} DESTINATION
		lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	target_link_libraries (${PLUGIN_NAME} elektra)
	target_link_libraries (${PLUGIN_NAME} ${YAJL_LIBRARIES})
	add_libraries (elektra-full ${YAJL_LIBRARIES})

	add_plugintest (yajl)
else (YAJL_FOUND)
	remove_plugin (yajl "yajl not found")
endif (YAJL_FOUND)
