add_lib (
	kdb
	SOURCES
	backends.c
	contracts.c
	hooks.c
	kdb.c
	plugin.c
	changetracking.c
	diff.c
	COMPONENT
	libelektra${SO_VERSION})
if (BUILD_SHARED)
	target_sources (elektra-kdb PRIVATE $<TARGET_OBJECTS:elektra-pluginload-dl>)
endif (BUILD_SHARED)
