#[[
include (LibAddPlugin)
include (SafeCheckSymbolExists)

if (DEPENDENCY_PHASE)
	plugin_check_if_included ("resolver")
	if (NOT_INCLUDED)
		remove_plugin (cache "resolver plugin not found (${NOT_INCLUDED})")
		return ()
	endif (NOT_INCLUDED)

	plugin_check_if_included ("mmapstorage")
	if (NOT_INCLUDED)
		remove_plugin (cache "mmapstorage plugin not found (${NOT_INCLUDED})")
		return ()
	endif (NOT_INCLUDED)

	add_definitions (-D_GNU_SOURCE -D_DARWIN_C_SOURCE)
	safe_check_symbol_exists (nftw "ftw.h" HAVE_NFTW)
	if (NOT HAVE_NFTW)
		remove_plugin (cache "nftw (ftw.h) not found")
		return ()
	endif (NOT HAVE_NFTW)

	set (CACHE_DEPENDENCIES_OK PARENT_SCOPE)
endif ()

add_plugin (
	cache
	SOURCES cache.h cache.c
	LINK_ELEKTRA elektra-kdb
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION})

if (CACHE_DEPENDENCIES_OK AND BUILD_SHARED)
	add_dependencies (elektra-cache elektra-resolver_fm_hpu_b elektra-mmapstorage)
endif ()

unset (CACHE_DEPENDENCIES_OK)
]]
