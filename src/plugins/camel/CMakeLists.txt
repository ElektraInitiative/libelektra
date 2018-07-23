include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (getline "stdio.h" HAVE_GETLINE)
if (NOT HAVE_GETLINE)
	remove_plugin (camel "requires getline")
endif ()

add_plugin (camel SOURCES camel.h camel.c LINK_ELEKTRA elektra-ease ADD_TEST INSTALL_TEST_DATA TEST_README)
