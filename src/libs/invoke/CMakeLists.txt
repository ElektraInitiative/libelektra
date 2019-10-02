file (GLOB SOURCES *.c)
add_lib (invoke
	 SOURCES
	 ${SOURCES}
	 LINK_ELEKTRA
	 elektra-kdb)
