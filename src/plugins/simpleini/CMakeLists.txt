include (LibAddMacros)

include (CheckSymbolExists)
check_symbol_exists (__GNU_LIBRARY__ "features.h" HAVE_GLIBC)

if (HAVE_GLIBC)
	add_plugin (
		simpleini
		SOURCES simpleini.h simpleini.c
		LINK_ELEKTRA elektra-ease elektra-utility COMPONENT libelektra${SO_VERSION}
		ADD_TEST)
else ()
	remove_plugin (simpleini "simpleini needs glibc to work, use the mini plugin instead")
endif ()
