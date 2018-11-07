file (GLOB SOURCES
	   *.c)
add_lib (highlevel SOURCES ${SOURCES} elektra_conversion_doxygen.h LINK_ELEKTRA elektra-core elektra-kdb elektra-ease)
