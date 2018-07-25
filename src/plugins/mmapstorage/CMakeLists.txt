include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	set (plugin mmapstorage)
	find_package (ZLIB)
	if (ENABLE_CHECKSUM AND NOT ZLIB_FOUND)
		remove_plugin (${plugin} "zlib development files not found")
	endif ()

	# clean up for dependency phase
	unset (plugin)
endif ()

if (ENABLE_OPTIMIZATIONS)
	set (ELEKTRA_ENABLE_OPTIMIZATIONS "1")
endif (ENABLE_OPTIMIZATIONS)

if (ENABLE_CHECKSUM)
	add_plugin (mmapstorage
		    SOURCES mmapstorage.h
			    mmapstorage.c
		    INCLUDE_DIRECTORIES ${ZLIB_INCLUDE_DIRS}
		    LINK_LIBRARIES ${ZLIB_LIBRARIES}
		    ADD_TEST)
else (ENABLE_CHECKSUM)
	add_plugin (mmapstorage SOURCES mmapstorage.h mmapstorage.c ADD_TEST)
endif (ENABLE_CHECKSUM)
