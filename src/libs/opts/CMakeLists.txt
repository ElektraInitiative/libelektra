file (GLOB SOURCES *.c)

add_lib (opts SOURCES ${SOURCES} LINK_ELEKTRA elektra-meta COMPONENT libelektra4)
