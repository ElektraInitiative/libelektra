include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (strdup "string.h" HAVE_STRDUP)
if (NOT HAVE_STRDUP)
	remove_plugin(glob "requires strdup")
endif (NOT HAVE_STRDUP)

add_plugin (glob SOURCES glob.h glob.c ADD_TEST)
