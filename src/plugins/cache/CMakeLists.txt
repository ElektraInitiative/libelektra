include (LibAddPlugin)
include (SafeCheckSymbolExists)

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

safe_check_symbol_exists (nftw "ftw.h" HAVE_NFTW)

if (NOT HAVE_NFTW)
	message ("nftw (ftw.h) not found, excluding the cache plugin")
	return ()
endif (NOT HAVE_NFTW)

add_plugin (cache
	    SOURCES cache.h cache.c
	    LINK_ELEKTRA elektra-kdb
	    ADD_TEST TEST_README)

if (DEPENDENCY_PHASE AND BUILD_SHARED)
	add_dependencies (elektra-cache elektra-resolver_fm_hpu_b elektra-mmapstorage)
endif ()
