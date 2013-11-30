find_package(Yajl REQUIRED)
if (YAJL_FOUND)
	include (LibAddMacros)

	add_includes (elektra-full ${YAJL_INCLUDE_DIR})
	include_directories (${YAJL_INCLUDE_DIR})

	set (SOURCES yajl.c array.c iterator.c yajl_gen.c
		yajl_gen_open.c  yajl_gen_close.c
		yajl_parse.c name.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	file (COPY examples
		DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

	add_library (elektra-yajl MODULE ${SOURCES})
	install (TARGETS elektra-yajl DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	target_link_libraries (elektra-yajl ${YAJL_LIBRARIES})
	add_libraries (elektra-full ${YAJL_LIBRARIES})

	add_plugintest (yajl)
else (YAJL_FOUND)
	remove_plugin (yajl "yajl not found")
endif (YAJL_FOUND)
