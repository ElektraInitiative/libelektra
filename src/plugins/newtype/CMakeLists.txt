add_plugin (newtype
	    SOURCES newtype.h
		    newtype.c
		    types.h
		    types.c
	    LINK_ELEKTRA elektra-highlevel
			 elektra-meta
			 elektra-ease
	    ADD_TEST
	    TEST_README)
