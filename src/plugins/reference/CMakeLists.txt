include (LibAddMacros)

add_plugin (reference
	    SOURCES reference.h
			reference.c
			referencegraph.c
			referencegraph.h
		LINK_ELEKTRA elektra-ease
	    ADD_TEST
	    TEST_README)
