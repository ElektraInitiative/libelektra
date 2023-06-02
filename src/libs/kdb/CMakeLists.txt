set (
	SOURCES
	backends.c
	contracts.c
	hooks.c
	kdb.c
	plugin.c
	changetracking.c
	diff.c)

add_lib (kdb SOURCES ${SOURCES} COMPONENT libelektra${SO_VERSION})
if (BUILD_SHARED)
	target_link_libraries (elektra-kdb $<TARGET_OBJECTS:elektra-pluginload>)
endif (BUILD_SHARED)
