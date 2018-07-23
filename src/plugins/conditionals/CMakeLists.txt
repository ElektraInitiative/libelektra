include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (strdup "string.h" HAVE_STRDUP)
if (NOT HAVE_STRDUP)
	remove_plugin(conditionals "could not find strdup")
endif (NOT HAVE_STRDUP)

add_plugin (
	conditionals SOURCES conditionals.h conditionals.c LINK_ELEKTRA elektra-meta ADD_TEST TEST_README TEST_REQUIRED_PLUGINS dump ini ni)
