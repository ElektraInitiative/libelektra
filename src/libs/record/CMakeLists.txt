file (GLOB SOURCES *.c)
file (GLOB HEADERS *.h)
add_lib (record SOURCES ${SOURCES} ${HEADERS} COMPONENT libelektra${SO_VERSION})
