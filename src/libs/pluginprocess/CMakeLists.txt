find_package (Pluginprocess)

file (GLOB SOURCES
	   *.c)

if (PLUGINPROCESS_FOUND)
	add_lib (pluginprocess SOURCES ${SOURCES} LINK_ELEKTRA elektra elektra-invoke elektra-plugin)
else (PLUGINPROCESS_FOUND)
	message ("${PLUGINPROCESS_NOTFOUND_INFO}, excluding pluginprocess library")
endif (PLUGINPROCESS_FOUND)
