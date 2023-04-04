file (GLOB SOURCES *.c)
file (GLOB HEADERS *.h)
add_lib (record
	SOURCES
	${SOURCES}
	${HEADERS}
	LINK_ELEKTRA
	elektra-kdb
	COMPONENT
	libelektra${SO_VERSION})
