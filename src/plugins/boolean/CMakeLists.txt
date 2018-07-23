include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (strdup "string.h" HAVE_STRDUP)
check_symbol_exists (strtok_r "string.h" HAVE_STRTOK_R)
if (NOT (HAVE_STRDUP AND HAVE_STRTOK_R))
	remove_plugin (boolean "requires strdup and strtok_r")
endif ()

add_plugin (boolean SOURCES boolean.h boolean.c ADD_TEST TEST_README)
