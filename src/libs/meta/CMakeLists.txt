file (GLOB SOURCES *.c)
add_lib (
	meta
	SOURCES
	${SOURCES}
	LINK_ELEKTRA
	elektra-ease
	COMPONENT
	libelektra${SO_VERSION})
