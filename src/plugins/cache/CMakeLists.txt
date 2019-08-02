include (LibAddPlugin)

plugin_check_if_included ("resolver")

if (NOT_INCLUDED)
	message ("resolver plugin not found (${NOT_INCLUDED}), excluding the cache plugin")
	return ()
endif (NOT_INCLUDED)

plugin_check_if_included ("mmapstorage")

if (NOT_INCLUDED)
	message ("mmapstorage plugin not found (${NOT_INCLUDED}), excluding the cache plugin")
	return ()
endif (NOT_INCLUDED)

add_plugin (cache
	    SOURCES cache.h cache.c
	    LINK_ELEKTRA elektra-kdb
	    ADD_TEST TEST_README)

if (DEPENDENCY_PHASE AND BUILD_SHARED)
	add_dependencies (elektra-cache elektra-resolver_fm_hpu_b elektra-mmapstorage)
endif ()
