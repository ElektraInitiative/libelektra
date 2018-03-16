include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	# Plugin variant: crypto_openssl
	set (plugin mmapstorage)
	find_package (ZLIB)
	if (NOT ZLIB_FOUND)
		remove_plugin ( ${plugin} "zlib development files not found")
	endif ()


	# clean up for dependency phase
	unset (plugin)
endif ()

add_plugin (mmapstorage
	SOURCES
		mmapstorage.h
		mmapstorage.c
	INCLUDE_DIRECTORIES
		${ZLIB_INCLUDE_DIRS}
	LINK_LIBRARIES
		${ZLIB_LIBRARIES}
	ADD_TEST
)
