find_package (Pluginprocess)

file (GLOB SOURCES *.c)

if (PLUGINPROCESS_FOUND)
	add_lib (pluginprocess
		 SOURCES
		 ${SOURCES}
		 LINK_ELEKTRA
		 elektra-invoke
		 elektra-plugin)

	if (ENABLE_TESTING)
		add_subdirectory (tests)
	endif (ENABLE_TESTING)
else (PLUGINPROCESS_FOUND)
	message ("${PLUGINPROCESS_NOTFOUND_INFO}")
endif (PLUGINPROCESS_FOUND)
