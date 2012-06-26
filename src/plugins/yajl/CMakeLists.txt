find_package(Yajl REQUIRED)
if (YAJL_FOUND)
	include (LibAddMacros)

	add_includes (elektra-full ${YAJL_INCLUDE_DIR})
	include_directories (${YAJL_INCLUDE_DIR})

	set (SOURCES yajl.h yajl.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	add_library (elektra-yajl MODULE ${SOURCES})
	install (TARGETS elektra-yajl DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	target_link_libraries (elektra-yajl ${YAJL_LIBRARIES})
	add_libraries (elektra-full ${YAJL_LIBRARIES})

	add_plugintest (yajl)
else (YAJL_FOUND)
	remove_plugin (yajl "yajl not found")
endif (YAJL_FOUND)
