file (GLOB SOURCES *.c)
add_lib (pluginprocess
	SOURCES
		${SOURCES}
	LINK_ELEKTRA
		elektra
		elektra-invoke
		elektra-plugin
)
