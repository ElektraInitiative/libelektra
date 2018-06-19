include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	set (plugin mmapstorage)
	find_package (ZLIB)
	if (NOT ZLIB_FOUND)
		remove_plugin (${plugin} "zlib development files not found")
	endif ()

	# clean up for dependency phase
	unset (plugin)
endif ()

# for zlib/crc: INCLUDE_DIRECTORIES ${ZLIB_INCLUDE_DIRS} LINK_LIBRARIES ${ZLIB_LIBRARIES}
add_plugin (mmapstorage SOURCES mmapstorage.h mmapstorage.c ADD_TEST)
