include (LibAddMacros)

add_plugin (directoryvalue
	    SOURCES directoryvalue.h
		    directoryvalue.c
	    LINK_ELEKTRA elektra-ease
	    ADD_TEST
	    TEST_README)
