include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (strdup "string.h" HAVE_STRDUP)
if (NOT HAVE_STRDUP)
	remove_plugin(directoryvalue "requires strdup")
endif (NOT HAVE_STRDUP)

add_plugin (directoryvalue SOURCES directoryvalue.h directoryvalue.c LINK_ELEKTRA elektra-ease ADD_TEST TEST_README)
