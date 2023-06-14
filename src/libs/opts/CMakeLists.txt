file (GLOB SOURCES *.c)

add_lib (
	opts
	SOURCES
	${SOURCES}
	LINK_ELEKTRA
	elektra-meta
	elektra-types
	COMPONENT
	libelektra${SO_VERSION})
