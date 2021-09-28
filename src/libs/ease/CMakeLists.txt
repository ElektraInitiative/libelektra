file (GLOB SOURCES *.c)
file (GLOB HEADERS *.h)
add_lib (ease SOURCES ${SOURCES} ${HEADERS} COMPONENT libelektra${SO_VERSION})
