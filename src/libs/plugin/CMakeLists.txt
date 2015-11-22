file (GLOB SOURCES *.c)
add_lib (plugin
		SOURCES ${SOURCES}
		LINK_LIBRARIES elektra-kdb
	)
