include (LibAddMacros)

if (DEPENDENCY_PHASE)
	set (plugin mmapstorage)
	find_package (ZLIB)
	if (ENABLE_CHECKSUM AND NOT ZLIB_FOUND)
		remove_plugin (${plugin} "zlib development files not found")
	endif ()

	# clean up for dependency phase
	unset (plugin)
endif (DEPENDENCY_PHASE)

if (ENABLE_CHECKSUM)
	add_plugin (mmapstorage
		    SOURCES mmapstorage.h
			    mmapstorage.c
		    INCLUDE_DIRECTORIES ${ZLIB_INCLUDE_DIRS}
		    LINK_LIBRARIES ${ZLIB_LIBRARIES}
		    LINK_ELEKTRA elektra-core
		    ADD_TEST)
else (ENABLE_CHECKSUM)
	add_plugin (mmapstorage
		    SOURCES mmapstorage.h
			    mmapstorage.c
		    LINK_ELEKTRA elektra-core
		    ADD_TEST)
endif (ENABLE_CHECKSUM)
