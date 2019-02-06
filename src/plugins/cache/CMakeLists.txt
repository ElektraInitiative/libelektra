include (LibAddMacros)

add_plugin (cache
	    SOURCES cache.h
		    cache.c
	    LINK_ELEKTRA elektra-kdb
	    ADD_TEST
	    TEST_README)
