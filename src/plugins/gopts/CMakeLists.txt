include (LibAddMacros)

add_plugin (gopts
	    SOURCES gopts.h
		    gopts.c
	    LINK_ELEKTRA elektra-opts
	    ADD_TEST
	    TEST_README)
