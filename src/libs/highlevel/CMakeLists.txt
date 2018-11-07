file (GLOB SOURCES
	   *.c)
add_lib (highlevel SOURCES ${SOURCES} LINK_ELEKTRA elektra-core elektra-kdb elektra-ease)
