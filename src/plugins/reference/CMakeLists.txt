include (LibAddMacros)

add_plugin (reference
	    SOURCES reference.h
		    reference.c
		LINK_ELEKTRA elektra-ease
	    ADD_TEST
	    TEST_README)
