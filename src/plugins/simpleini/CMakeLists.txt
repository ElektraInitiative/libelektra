include (LibAddMacros)

include (CheckSymbolExists)
check_symbol_exists (__GNU_LIBRARY__ "features.h"    HAVE_GLIBC)

# silently ignore plugin on non-glibc systems
if (HAVE_GLIBC)
	add_plugin (simpleini
		SOURCES
			simpleini.h
			simpleini.c
		LINK_ELEKTRA
			elektra-ease
		)
endif()
