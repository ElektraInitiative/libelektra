include (CheckSymbolExists)
include (LibAddPlugin)

check_symbol_exists (strdup "string.h" HAVE_STRDUP)
if (NOT HAVE_STRDUP)
	remove_plugin(base64 "requires strdup")
endif (NOT HAVE_STRDUP)

add_plugin (base64
	    SOURCES base64_functions.h
		    base64_functions.c
		    base64.h
		    base64.c
	    COMPILE_DEFINITIONS ELEKTRA_PLUGIN_NAME=\"base64\"
				ELEKTRA_PLUGIN_NAME_C=base64
	    ADD_TEST
	    TEST_README
	    TEST_REQUIRED_PLUGINS ini)
