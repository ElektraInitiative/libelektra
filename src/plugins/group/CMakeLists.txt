include (LibAddMacros)

add_plugin (group
	    SOURCES group.h
		    group.c
        LINK_ELEKTRA elektra-ease
	    ADD_TEST
	    TEST_README)
