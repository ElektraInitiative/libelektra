file (GLOB SOURCES
	   *.c)
add_lib (globbing SOURCES ${SOURCES} LINK_ELEKTRA elektra-ease)
